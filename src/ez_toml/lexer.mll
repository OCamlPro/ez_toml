{
(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

  (* Directly imported from toml.7.0.1 *)

  (*[@@@warning "-26"]*)
open Parser
}

let t_white   = ['\t' ' ']
let t_eol     = '\n'|"\r\n"
let t_digit   = ['0'-'9']
let t_int_part  = '0' | ['1'-'9'] ('_'? t_digit+)*
let t_fractional_int_part  = t_digit ('_'? t_digit+)* (** Leading zeros are not allowed *)
let t_sign    = ['-''+']
let t_int     = t_sign? t_int_part
let t_frac    = '.' t_fractional_int_part
let t_exp     = ['E''e'] t_int
let t_float   = t_sign? t_int_part ((t_frac t_exp?) | t_exp)
let t_bool    = ("true"|"false")
let t_key     = ['A'-'Z''a'-'z''0'-'9''_''-']+

let t_date    =
	t_digit t_digit t_digit t_digit
    '-' t_digit t_digit
    '-' t_digit t_digit
    (['T' 't']
     t_digit t_digit
     ':' t_digit t_digit
     ':' t_digit t_digit ('.' t_digit+)?
     (['Z' 'z'] | (['+' '-'] t_digit t_digit ':' t_digit t_digit)))?

(** RFC 3339 date of form 1979-05-27T07:32:00.42+00:00 *)

let t_escape  =  '\\' ['b' 't' 'n' 'f' 'r' '"' '\\']
let t_alpha   = ['A'-'Z' 'a'-'z']
let t_alphanum= t_alpha | t_digit
let t_unicode = t_alphanum t_alphanum t_alphanum t_alphanum

rule tomlex = parse
| t_int as value   {
  INTEGER (value)}
|  ( '0' 'x' [ '0'-'9' 'A'-'F' 'a'-'f']+ ) as value   {
  INTEGER (value)}
| t_float as value   { FLOAT (value) }
| t_bool as value  { BOOL (bool_of_string value) }
| t_date as date { DATE date}
| t_white+ { tomlex lexbuf }
| t_eol { Internal.update_loc lexbuf;tomlex lexbuf }
| '=' { EQUAL }

(* Only for Drom: we need to be able to combine values in templates *)
| ":=" { SET }
| "==" { INIT }
| "-=" { CLEAR }
(* :Only for drom *)

| '[' { LBRACK }
| ']' { RBRACK }
| '{' { LBRACE }
| '}' { RBRACE }
| '"' '"' '"' (t_eol? as eol) {
    if eol <> "" then Internal.update_loc lexbuf ;
    multiline_string (Buffer.create 13) lexbuf }
| '"' { basic_string (Buffer.create 13) lexbuf }
| '\'' { literal_string (Buffer.create 13) lexbuf }
| "'''" (t_eol? as eol) {
    if eol <> "" then Internal.update_loc lexbuf ;
    multiline_literal_string (Buffer.create 13) lexbuf }
| ',' { COMMA }
| '.' { DOT }
| '#' (_ # [ '\n' '\r' ] )* { tomlex lexbuf }
| t_key as value { KEY (value) }
| eof   { EOF }

and literal_string buff = parse
| '\''   { STRING_INLINE (Internal_types.Literal, Buffer.contents buff)}
| _ as c {
  Buffer.add_char buff c ;
  literal_string buff lexbuf }

and multiline_literal_string buff = parse
  | "'''"  { STRING_MULTILINE (Internal_types.LiteralMultiline,
                               Buffer.contents buff)}
| t_eol as eol {
  Buffer.add_string buff eol ;
  Internal.update_loc lexbuf ;
  multiline_literal_string buff lexbuf }
| _ as c {
  Buffer.add_char buff c ;
  multiline_literal_string buff lexbuf }

and basic_string buff = parse
| '"'  { STRING_INLINE (Internal_types.Quoted, Buffer.contents buff) }
| ""   { string_common basic_string buff lexbuf }

and multiline_string buff = parse
  | '"' '"' '"' { STRING_MULTILINE (Internal_types.QuotedMultiline,
                                    Buffer.contents buff) }
| '\\' t_eol {
  Internal.update_loc lexbuf;
  multiline_string_trim buff lexbuf }
| t_eol as eol {
  Internal.update_loc lexbuf;
  Buffer.add_string buff eol;
  multiline_string buff lexbuf }
| "" { string_common multiline_string buff lexbuf }

and multiline_string_trim buff = parse
| t_eol {
  Internal.update_loc lexbuf;
  multiline_string_trim buff lexbuf }
| t_white { multiline_string_trim buff lexbuf }
| "" { multiline_string buff lexbuf }

and string_common next buff = parse
| t_escape as value {
  Buffer.add_string buff (Scanf.unescaped value);
  next buff lexbuf }
| "\\u" (t_unicode as u) {
  Buffer.add_string buff (Unicode.to_utf8 u);
  next buff lexbuf }
| '\\' { failwith "Forbidden escaped char" }
| eof  { failwith "Unterminated string" }
| _ as c {
  let code = Char.code c in
  if code < 16 then
    failwith "Control characters (U+0000 to U+001F) must be escaped";
  Buffer.add_char buff c;
  next buff lexbuf }

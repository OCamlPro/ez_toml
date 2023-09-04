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

(* open EzCompat *)
open Ez_file.V1

module Types = Types

open Types

let of_string ?file string =
  let lexbuf = Lexing.from_string string in
  begin
    match file with
    | None -> ()
    | Some file ->
        Lexing.set_filename lexbuf file
  end;
  try
    Parser.toml Lexer.tomlex lexbuf
  with
  | Parser.Error ->
      let loc = Lexer.loc_of_lexbuf lexbuf in
      raise (Error (loc, Parse_error))
  | Failure msg ->
      let loc = Lexer.loc_of_lexbuf lexbuf in
      raise (Error (loc, Syntax_error msg))

let of_file file =
  let s = EzFile.read_file file in
  of_string ~file s

let to_string table = Printer.string_of_table table
let to_file table file =
  let s = to_string table in
  EzFile.write_file file s

let string_of_error error =
  match error with
  | Parse_error -> "Parse error"
  | Syntax_error msg -> Printf.sprintf "Syntax error: %s" msg

let string_of_location loc =
  Printf.sprintf "File %S, line %s, chars %s"
    loc.file
    ( if loc.line_begin = loc.line_end then
        string_of_int loc.line_begin
      else
        Printf.sprintf "%d-%d" loc.line_begin loc.line_end )
    ( if loc.char_begin = loc.char_end then
        string_of_int loc.char_begin
      else
        Printf.sprintf "%d-%d" loc.char_begin loc.char_end )


include Misc

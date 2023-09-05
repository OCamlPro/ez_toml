%{
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

    open Internal_types
%}

(* OcamlYacc definitions *)
%token <bool> BOOL
%token <string> INTEGER
%token <string> FLOAT
%token <Internal_types.string_format * string> STRING_INLINE
%token <Internal_types.string_format * string> STRING_MULTILINE
%token <string> DATE
%token <string> KEY
%token LBRACK RBRACK LBRACE RBRACE EOF COMMA DOT
%token EQUAL INIT SET CLEAR

%start toml

%type <Internal_types.line list> toml

%%
(* Grammar rules *)
toml:
 | keysValue* group* EOF
   { $1 @ List.flatten $2 }

group:
   | group_header keysValue* { $1 :: $2 }

group_header:
   | LBRACK LBRACK key_path RBRACK RBRACK {
       Internal.line $sloc @@ Array_item $3 }
   | LBRACK key_path RBRACK               {
       Internal.line $sloc @@ Table_item $2 }

key:
 | STRING_INLINE { snd $1 }
 | KEY    { $1 }
 | INTEGER    { $1 }

key_path: k = separated_nonempty_list (DOT, key) { k }

op:
 | EQUAL { OpEqual }
 | INIT { OpInit }
 | SET { OpSet }
 | CLEAR { OpUnset }

keysValue:
 | key_path_loc op value_loc { Internal.line $sloc @@ Set
                                               { bind_var = $1 ;
                                                 bind_op = $2 ;
                                                 bind_val = $3 } }

value:
    BOOL { IBool($1) }
  | INTEGER { IInt $1 }
  | FLOAT { IFloat $1 }
  | STRING_INLINE { let (format, s) = $1 in IString(format,s) }
  | STRING_MULTILINE { let (format, s) = $1 in IString(format,s) }
  | DATE { IDate $1 }
  | LBRACK RBRACK { IArray [] }
  | LBRACK value_loc array_end { IArray ( $2 :: $3 ) }
  | LBRACE separated_list(COMMA,
                          keys_set_value) RBRACE { ITable $2 }

key_path_loc :
  | key_path { Internal.loc $sloc $1 }

value_loc:
  | value { Internal.loc $sloc $1 }

keys_set_value:
  | key_path_loc EQUAL value_loc   { { bind_var = $1 ;
                                       bind_op = OpEqual ;
                                       bind_val = $3 } }

array_end:
    COMMA value_loc array_end { $2 :: $3 }
  | COMMA? RBRACK { [] }

%%

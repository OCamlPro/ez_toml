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

open EzCompat

type key = string
type key_path = key list

type location = {
  file : string ;
  mutable line_begin : int ;
  mutable line_end : int ;
  mutable char_begin : int ;
  mutable char_end : int ; (* in line *)
}

type error =
  | Parse_error                                   (* error 0 *)
  | Syntax_error of string                        (* error 1 *)
  | Invalid_lookup                                (* error 2 *)
  | Invalid_lookup_in_inline_array                (* error 3 *)
  | Key_already_exists of key_path                (* error 4 *)
  | Invalid_key_set of key                        (* error 5 *)
  | Invalid_table of key_path                     (* error 6 *)
  | Append_item_to_non_array of key_path          (* error 7 *)
  | Append_item_to_non_table_array of key_path    (* error 8 *)
  | Invalid_escaped_unicode of string             (* error 9 *)
  | Expected_error_before_end_of_file of int      (* error 10 *)
  | Expected_error_did_not_happen of int          (* error 11 *)
  | Expected_error_but_another_error of int * location * int * error
  (* error 12 *)
  | Forbidden_escaped_character                   (* error 13 *)
  | Unterminated_string                           (* error 14 *)
  | Control_characters_must_be_escaped of char    (* error 15 *)
  | Duplicate_table_item of key_path              (* error 16 *)

exception Error of location * int * error

type format =
  | Any       (* Guess automatically. Usually, use block format *)
  | Inline    (* Put the value on a single line *)
  | Multiline (* Put the value on multiple lines if possible *)
  | Literal   (* Use literal quotes (only for strings) *)

type config = {
  allow_override : bool ; (* allows setting value twice *)
  allow_extops : bool ;   (* allows additional operators :=, ==, -= *)
  pedantic : bool ;       (* complains about spec errors *)
  newline : string ;      (* newline to be used *)
}


type table = node StringMap.t

and node = {
  node_comment_before : string list ;  (* comments on preceeding lines *)
  mutable node_comment_after : string option ; (* comment at end of line *)
  node_loc : location ;
  node_format : format ;
  node_pos : int ;     (* a position *)
  mutable node_value : value ;
  node_name : key_path ;
}

and value =
  | Table of table
  | Array of node array
  | String of string
  | Bool of bool
  | Int of string
  | Float of string
  | Date of string

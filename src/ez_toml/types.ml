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
  | Parse_error
  | Syntax_error of string
  | Invalid_lookup
  | Invalid_lookup_in_inline_array
  | Key_already_exists of key_path
  | Invalid_key_set of key
  | Invalid_table of key_path

exception Error of location * error

type format =
  | Any       (* Guess automatically. Usually, use block format *)
  | Inline    (* Put the value on a single line *)
  | Multiline (* Put the value on multiple lines if possible *)
  | Literal   (* Use literal quotes (only for strings) *)

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

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

type location = {
  file : string ;
  line_begin : int ;
  line_end : int ;
  char_begin : int ;
  char_end : int ; (* in line *)
}

type error =
  | Parse_error of location

type format =
  | Any       (* Guess automatically. Usually, use block format *)
  | Inline    (* Put the value on a single line *)
  | Multiline (* Put the value on multiple lines if possible *)
  | Literal   (* Use literal quotes (only for strings) *)

type table = node StringMap.t

and node = {
  node_comment_before : string list ;  (* comments on preceeding lines *)
  node_comment_after : string option ; (* comment at end of line *)
  node_loc : location ;
  node_format : format ;
  node_pos : int ;     (* a position *)
  node_value : value ;
}

and value =
  | Table of table
  | Array of node array
  | String of string
  | Bool of bool
  | Int of string
  | Float of string
  | Date of string

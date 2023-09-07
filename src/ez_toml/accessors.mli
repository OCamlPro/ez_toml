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

open Types

val get : node -> key_path -> node
val set : ?config:config -> node -> key_path -> value:node -> unit

val type_of_node : node -> string

(* customizing nodes *)
val add_comments : node -> string list -> unit
val add_eol_comment : node -> string -> unit

(* to create generic nodes, use `V1.node` *)
val string : string -> node
val bool : bool -> node
val int : int -> node
val float : float -> node
val date : float -> node
val array : node array -> node
val table : table -> node
val table_of_list : ( string * node ) list -> node

(* extractors *)

val extract_string : node -> string
val extract_bool : node -> bool
val extract_int : node -> int
val extract_float : node -> float
val extract_date : node -> float
val extract_array : node -> node array
val extract_table : node -> table

val table_iter : node -> ( string -> node -> unit ) -> unit
val array_iteri : node -> ( int -> node -> unit ) -> unit
val array_length : node -> int

(* low-level value access *)

val get_value : node -> value
val type_of_value : value -> string
val set_value : node -> value -> unit

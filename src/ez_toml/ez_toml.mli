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

module DEPRECATED = Drom_toml

module Types = Types
open Types

val of_string : ?file:string -> string -> table
val of_file : string -> table

val to_string : table -> string
val to_file : table -> string -> unit

val string_of_error : error -> string

(* useful to build values *)

val noloc : location
val node : ?pos:int -> ?format:Types.format ->
  ?loc:location -> ?before:string list -> ?after:string ->
  value -> node

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

module TOML : sig

  module Types = Types
  open Types

  val default_config : config

  val of_string : ?file:string -> ?config:config -> string -> node
  val of_file : ?config:config -> string -> node

  val to_string : node -> string
  val to_file : node -> string -> unit

  (* useful to build values *)

  val noloc : location
  val node : ?format:Types.format ->
    ?loc:location -> ?before:string list ->
    ?name:key_path -> ?after:string ->
    value -> node

  include ( module type of Printer )
  include ( module type of Accessors )

end

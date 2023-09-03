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

module DEPRECATED = Drom_toml

module Types = Types

let of_string ?file string =
  let lexbuf = Lexing.from_string string in
  begin
    match file with
    | None -> ()
    | Some file ->
        Lexing.set_filename lexbuf file
  end;
  (* Parser.toml Lexer.tomlex lexbuf *)
  assert false (* TODO *)

let of_file file =
  let s = EzFile.read_file file in
  of_string ~file s

let to_string table = Printer.string_of_table table
let to_file table file =
  let s = to_string table in
  EzFile.write_file file s

let string_of_error _error = assert false

include Misc

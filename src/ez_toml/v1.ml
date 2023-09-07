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

module TOML = struct

  open Ez_file.V1

  module Types = Types

  open Types

  include Printer
  include Accessors

  let default_config = Internal_misc.default_config

  let of_string ?file ?(config = default_config) string =
    Internal_lexing.init ();
    let lexbuf = Lexing.from_string string in
    begin
      match file with
      | None -> ()
      | Some file ->
          (* Lexing.set_filename lexbuf file available at 4.11 *)
          let open Lexing in
          lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file}
    end;
    try
      let lines = Parser.toml Lexer.tomlex lexbuf in
      (*      Internal.eprint_lines lines; *)
      let loc = Internal_lexing.loc_of_lexbuf lexbuf in
      let node = Internal_parsing.table_of_lines ~loc config lines in
      let loc2 = Internal_lexing.loc_of_lexbuf lexbuf in
      Internal_lexing.expand_loc loc loc2;
      node
    with
    | Parser.Error ->
        let loc = Internal_lexing.loc_of_lexbuf lexbuf in
        Internal_misc.error ~loc 0 Parse_error
    | Failure msg ->
        let loc = Internal_lexing.loc_of_lexbuf lexbuf in
        Internal_misc.error ~loc 1 ( Syntax_error msg )

  let of_file ?config file =
    let s = EzFile.read_file file in
    of_string ?config ~file s

  let to_string node = Printer.string_of_node node
  let to_file node file =
    let s = to_string node in
    EzFile.write_file file s

  let noloc = Internal_misc.noloc
  let node = Internal_misc.node

end

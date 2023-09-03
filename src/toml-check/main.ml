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

open Ezcmd.V2
open EZCMD.TYPES

type output_format = JSON | TOML | NONE

let rec json_of_table table =
  let list = ref [] in
  Ez_toml.DEPRECATED.Types.Table.iter (fun key value ->
      let key = Ez_toml.DEPRECATED.Types.Table.Key.to_string key in
      let value = json_of_value value in
      list := ( key, value ) :: !list
    ) table;
  let value = `O (List.rev !list) in
  ( value :> Ezjsonm.t )

and json_of_value value =
  match value with
  | Ez_toml.DEPRECATED.Types.TString s -> `String s
  | TBool bool -> `Bool bool
  | TFloat float -> `Float float
  | TInt int -> `Float (float_of_int int)
  | TDate date -> `Float date
  | TTable table -> ( json_of_table table :> Ezjsonm.value )
  | TArray array -> ( json_of_array array :> Ezjsonm.value )

and json_of_array array =
  let array =
    match array with
    | NodeEmpty -> []
    | NodeBool vs -> List.map (fun v -> `Bool v) vs
    | NodeFloat vs -> List.map (fun v -> `Float v) vs
    | NodeInt vs -> List.map (fun v -> `Float (float_of_int v)) vs
    | NodeDate vs -> List.map (fun v -> `Float v) vs
    | NodeString vs -> List.map (fun v -> `String v) vs
    | NodeTable vs -> ( List.map json_of_table vs :> Ezjsonm.value list )
    | NodeArray vs -> List.map json_of_array vs
  in
  `A array

let () =
  let output_format = ref JSON in
  let output_file = ref None in
  let files = ref [] in
  let action () =
    let output s =
      match !output_file with
      | None -> Printf.printf "%s\n%!" s
      | Some file ->
          Ez_file.V1.EzFile.write_file file s;
          output_file := None
    in
    List.iter (fun file ->
        if not (Sys.file_exists file) then begin
          Printf.eprintf "Error: file %S does not exist\n%!" file;
          exit 2
        end;
        Printf.eprintf "Parsing %S\n%!" file;
        match Ez_toml.DEPRECATED.Parser.from_filename file with
        | `Ok table ->
            begin
              match !output_format with
              | JSON ->
                  let json = json_of_table table in
                  let s = Ezjsonm.to_string ~minify:false json in
                  output s
              | TOML ->
                  let toml = Ez_toml.DEPRECATED.Printer.string_of_table table in
                  output toml
              | NONE -> Printf.eprintf "File %S parsed\n%!" file
            end
        | `Error (msg, _loc) ->
            Printf.eprintf "%s%!" msg
      ) !files;

  in
  let cmd = EZCMD.sub "main"
      ~args:[
        [],
        Arg.Anons (fun list -> files := list),
        EZCMD.info ~docv:"FILE" "Cobol file to parse";

        [ "to-json" ], Arg.Unit (fun () -> output_format := JSON),
        EZCMD.info "Output content to JSON format (default)";
        [ "to-toml" ], Arg.Unit (fun () -> output_format := TOML),
        EZCMD.info "Output content to TOML format";
        [ "no-output" ], Arg.Unit (fun () -> output_format := NONE),
        EZCMD.info "Do not output content, just check";
        [ "o" ], Arg.String (fun s -> output_file := Some s),
        EZCMD.info ~docv:"FILE" "Output format to FILE (default is stdout)";

      ]

      ~doc:"Read a TOML file"
      action

  in
  EZCMD.main cmd

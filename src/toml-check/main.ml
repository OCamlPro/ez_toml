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

open Ezcmd.V2
open EZCMD.TYPES

open Ez_toml.Types


let date_of_string s =
  fst (ISO8601.Permissive.datetime_tz ~reqtime:false s)

let string_of_date float =
  ISO8601.Permissive.string_of_datetimezone (float, 0.)

module TOML : sig

  val of_table : table -> Ez_toml.DEPRECATED.Types.table
  val to_table : Ez_toml.DEPRECATED.Types.table -> table

  val string_of_table : table -> string

  val of_filename : string -> table

end = struct

  open Ez_toml.DEPRECATED.Types

  exception UnsupportedMixedArray

  let rec of_table table =
    let t = ref Table.empty in
    StringMap.iter (fun key node ->
        t := Table.add ( Table.Key.of_string key ) (of_value node.node_value) !t
      ) table ;
    !t

  and of_value v =
    match v with
    | Bool v -> TBool v
    | String s -> TString s
    | Int s -> TInt ( int_of_string s )
    | Float s -> TFloat ( float_of_string s)
    | Date s -> TDate ( date_of_string s)
    | Table t -> TTable ( of_table t )
    | Array array -> TArray (of_array array)

  and of_array t =
    match t with
    | [||] -> NodeEmpty
    | array ->
        match array.(0).node_value with
        | Bool _ -> NodeBool ( Array.map (function
              { node_value = Bool b ; _ } -> b
            | _ -> raise UnsupportedMixedArray) array |> Array.to_list )
        | String _ -> NodeString ( Array.map (function
              { node_value = String b ; _ } -> b
            | _ -> raise UnsupportedMixedArray) array |> Array.to_list )
        | Int _ -> NodeInt ( Array.map (function
              { node_value = Int b ; _ } -> int_of_string b
            | _ -> raise UnsupportedMixedArray) array |> Array.to_list )
        | Float _ -> NodeFloat ( Array.map (function
              { node_value = Float b ; _ } -> float_of_string b
            | _ -> raise UnsupportedMixedArray) array |> Array.to_list )
        | Date _ -> NodeDate ( Array.map (function
              { node_value = Date b ; _ } -> date_of_string b
            | _ -> raise UnsupportedMixedArray) array |> Array.to_list )
        | Table _ -> NodeTable ( Array.map (function
              { node_value = Table b ; _ } -> of_table b
            | _ -> raise UnsupportedMixedArray) array |> Array.to_list )
        | Array _ -> NodeArray ( Array.map (function
              { node_value = Array b ; _ } -> of_array b
            | _ -> raise UnsupportedMixedArray) array |> Array.to_list )


  let rec to_table table =
    let t = ref StringMap.empty in
    Table.iter (fun key v ->
        t := StringMap.add ( Table.Key.to_string key )
            (Ez_toml.node (to_value v)) !t
      ) table ;
    !t

  and to_value v =
    match v with
    | TBool v -> Bool v
    | TString s -> String s
    | TInt s -> Int ( string_of_int s )
    | TFloat s -> Float ( string_of_float s)
    | TDate s -> Date ( string_of_date s)
    | TTable t -> Table ( to_table t )
    | TArray array -> Array (to_array array)

  and to_array t =
    match t with
    | NodeEmpty -> [||]
    | NodeBool t ->
        Array.of_list t |> Array.map (fun v -> Ez_toml.node @@ Bool v)
    | NodeString t ->
        Array.of_list t |> Array.map (fun v -> Ez_toml.node @@ String v)
    | NodeInt t ->
        Array.of_list t |> Array.map (fun v ->
            Ez_toml.node @@ Int ( string_of_int v))
    | NodeFloat t ->
        Array.of_list t |> Array.map (fun v ->
            Ez_toml.node @@ Float ( string_of_float v))
    | NodeDate t ->
        Array.of_list t |> Array.map (fun v ->
            Ez_toml.node @@ Date ( string_of_date v))
    | NodeTable t ->
        Array.of_list t |> Array.map (fun v ->
            Ez_toml.node @@ Table ( to_table v))
    | NodeArray t ->
        Array.of_list t |> Array.map (fun v ->
            Ez_toml.node @@ Array ( to_array v))

  let of_filename file =
    match Ez_toml.DEPRECATED.Parser.from_filename file with
    | `Ok table -> to_table table
    | `Error (msg, _loc) -> failwith msg

  let string_of_table table =
    Ez_toml.DEPRECATED.Printer.string_of_table ( of_table table )

end
type output_format = JSON | TOML | NONE

let rec json_of_table table =
  let list = ref [] in
  StringMap.iter (fun key value ->
      let value = json_of_value value.node_value in
      list := ( key, value ) :: !list
    ) table;
  let value = `O (List.rev !list) in
  ( value :> Ezjsonm.t )

and json_of_value value =
  match value with
  | String s -> `String s
  | Bool bool -> `Bool bool
  | Float float -> `Float ( float_of_string float )
  | Int int -> `Float (float_of_string int)
  | Date date -> `String date
  | Table table -> ( json_of_table table :> Ezjsonm.value )
  | Array array -> ( json_of_array array :> Ezjsonm.value )

and json_of_array array =
  let array = Array.map (fun node -> json_of_value node.node_value) array in
  `A ( Array.to_list array )

let () =
  let use_current_parser = ref true in
  let use_current_printer = ref true in
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
        match
          if !use_current_parser then
            Ez_toml.of_file file
          else
            TOML.of_filename file
        with
        | table ->
            begin
              match !output_format with
              | JSON ->
                  let json = json_of_table table in
                  let s = Ezjsonm.to_string ~minify:false json in
                  output s
              | TOML ->
                  let toml =
                    if !use_current_printer then
                      Ez_toml.to_string table
                    else
                      TOML.string_of_table table
                  in
                  output toml
              | NONE -> Printf.eprintf "File %S parsed\n%!" file
            end
        | exception Error (loc, msg) ->
            Printf.eprintf "Error at %s:\n" ( Ez_toml.string_of_location loc );
            Printf.eprintf "%s%!" ( Ez_toml.string_of_error msg ) ;
            exit 2
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

        [ "use-toml" ], Arg.Unit (fun () ->
            use_current_printer := false;
            use_current_parser := false),
        EZCMD.info "Use former toml library";
        [ "use-toml-parser" ], Arg.Unit (fun () ->
            use_current_parser := false),
        EZCMD.info "Use former toml library for parsing";
        [ "use-toml-printer" ], Arg.Unit (fun () ->
            use_current_printer := false),
        EZCMD.info "Use former toml library for printing";

      ]

      ~doc:"Read a TOML file"
      action

  in
  EZCMD.main cmd

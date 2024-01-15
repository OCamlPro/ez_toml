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
open Ez_file.V1

open Ezcmd.V2
open EZCMD.TYPES

open Ez_toml.V1

open TOML.Types


let date_of_string s =
  try
    fst (ISO8601.Permissive.datetime_tz ~reqtime:false s)
  with exn ->
    Printf.eprintf "date_of_string(%S) failed with exception %s\n%!"
      s (Printexc.to_string exn);
    exit 2

let string_of_date float =
  ISO8601.Permissive.string_of_datetimezone (float, 0.)

(* Former TOML library *)
module Toml : sig

  val string_of_node : node -> string
  val node_of_filename : string -> node

end = struct

  open Toml.Types

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
            (TOML.node (to_value v)) !t
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
        Array.of_list t |> Array.map (fun v -> TOML.node @@ Bool v)
    | NodeString t ->
        Array.of_list t |> Array.map (fun v -> TOML.node @@ String v)
    | NodeInt t ->
        Array.of_list t |> Array.map (fun v ->
            TOML.node @@ Int ( string_of_int v))
    | NodeFloat t ->
        Array.of_list t |> Array.map (fun v ->
            TOML.node @@ Float ( string_of_float v))
    | NodeDate t ->
        Array.of_list t |> Array.map (fun v ->
            TOML.node @@ Date ( string_of_date v))
    | NodeTable t ->
        Array.of_list t |> Array.map (fun v ->
            TOML.node @@ Table ( to_table v))
    | NodeArray t ->
        Array.of_list t |> Array.map (fun v ->
            TOML.node @@ Array ( to_array v))

  let node_of_filename file =
    match Toml.Parser.from_filename file with
    | `Ok table -> TOML.node @@ Table ( to_table table )
    | `Error (msg, _loc) -> failwith msg

  let string_of_node node =
    match of_value node.node_value with
    | TTable table -> Toml.Printer.string_of_table table
    | value -> Toml.Printer.string_of_value value

end
type output_format = JSON | TOML | NONE

module JSON : sig

  val of_node : node -> Ezjsonm.value
  val to_node : Ezjsonm.value -> node

end = struct

  let rec of_table table =
    let list = ref [] in
    StringMap.iter (fun key value ->
        let value = of_value value.node_value in
        list := ( key, value ) :: !list
      ) table;
    let value = `O (List.rev !list) in
    ( value :> Ezjsonm.t )

  and of_value value =
    match value with
    | String s -> `String s
    | Bool bool -> `Bool bool
    | Float float ->
        begin try
            `Float ( float_of_string float )
          with _ ->
            Printf.eprintf
              "Warning: invalid float %S detected in JSON conversion\n%!" float;
            `String (Printf.sprintf "INVALID FLOAT [%s]" float)
        end
    | Int int ->
        begin try
            `Float ( int_of_string int |> float_of_int )
          with _ ->
            Printf.eprintf
              "Warning: invalid int %S detected in JSON conversion\n%!" int;
            `String (Printf.sprintf "INVALID INT [%s]" int)
        end
    | Date date -> `String date
    | Table table -> ( of_table table :> Ezjsonm.value )
    | Array array -> ( of_array array :> Ezjsonm.value )

  and of_array array =
    let array = Array.map (fun node -> of_value node.node_value) array in
    `A ( Array.to_list array )

  let of_node node = of_value node.node_value

  let rec to_value json =
    match json with
    | `Bool b -> Bool b
    | `String s -> String s
    | `Float n -> Float ( string_of_float n )
    | `Null -> String "NULL"
    | `A list ->
        Array (List.map (fun value ->
            TOML.node @@ to_value value
          ) list |> Array.of_list )
    | `O list ->
        let table = ref StringMap.empty in
        List.iter (fun ( key, value ) ->
            table := StringMap.add key
                ( TOML.node @@ to_value value ) !table
          ) list ;
        Table !table

  let to_node json =
    TOML.node @@ to_value json

end

let () =
  let debug = ref false in
  let silent_errors = ref TOML.default_config.silent_errors in
  let use_current_parser = ref true in
  let use_current_printer = ref true in
  let output_format = ref JSON in
  let input_format = ref TOML in
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
          match !input_format with
          | NONE | TOML ->
              if !use_current_parser then
                let config = {
                  TOML.default_config
                  with
                    debug = !debug;
                    silent_errors = !silent_errors }
                in
                TOML.of_file ~config file
              else
                Toml.node_of_filename file
          | JSON ->
              let string = EzFile.read_file file in
              let json = Ezjsonm.from_string string in
              JSON.to_node json
        with
        | table ->
            begin
              match !output_format with
              | JSON ->
                  let json = JSON.of_node table in
                  let s = match json with
                    | `O _ as json -> Ezjsonm.to_string ~minify:false json
                    | _ -> assert false
                  in
                  output s
              | TOML ->
                  let toml =
                    if !use_current_printer then
                      TOML.to_string table
                    else
                      Toml.string_of_node table
                  in
                  output toml
              | NONE -> Printf.eprintf "File %S parsed\n%!" file
            end
        | exception Error (loc, n, msg) ->
            Printf.eprintf "%s:\nError %02d: %s\n%!"
              ( TOML.string_of_location loc )
              n
              ( TOML.string_of_error msg ) ;

            let ic = open_in loc.file in
            let rec iter i =
              match input_line ic with
              | line ->
                  if i >= loc.line_begin - 2 &&
                     i <= loc.line_end + 2 then begin

                    if i >= loc.line_begin && i <= loc.line_end then
                      Printf.eprintf "%-4d > %s\n" i line
                    else
                      Printf.eprintf "%-4d   %s\n" i line;
                    iter (i+1)
                  end else
                  if i < loc.line_begin then
                    iter (i+1)
              | exception _ -> ()
            in
            iter 1;
            close_in ic;
            exit 2
      ) !files;

  in
  let cmd = EZCMD.sub "main"
      ~args:[
        [],
        Arg.Anons (fun list -> files := list),
        EZCMD.info ~docv:"FILE" "Cobol file to parse";

        [ "of-json" ], Arg.Unit (fun () ->
            input_format := JSON;
            output_format := TOML),
        EZCMD.info "Output content to JSON format (default)";
        [ "to-json" ], Arg.Unit (fun () -> output_format := JSON),
        EZCMD.info "Output content to JSON format (default)";
        [ "to-toml" ], Arg.Unit (fun () -> output_format := TOML),
        EZCMD.info "Output content to TOML format";
        [ "no-output" ], Arg.Unit (fun () -> output_format := NONE),
        EZCMD.info "Do not output content, just check";
        [ "o" ], Arg.String (fun s -> output_file := Some s),
        EZCMD.info ~docv:"FILE" "Output format to FILE (default is stdout)";

        [ "run-tests" ], Arg.Unit Tests.run_and_exit,
        EZCMD.info "Run simple tests and exit";

        [ "use-toml" ], Arg.Unit (fun () ->
            use_current_printer := false;
            use_current_parser := false),
        EZCMD.info "Use former toml library";

        [ "use-toml-parser" ], Arg.Unit (fun () -> use_current_parser := false),
        EZCMD.info "Use former toml library for parsing";
        [ "use-toml-printer" ], Arg.Unit (fun () -> use_current_printer := false),
        EZCMD.info "Use former toml library for printing";

        [ "debug" ], Arg.Set debug,
        EZCMD.info "Print additional debug info";

        [ "error" ], Arg.String (fun s ->
            try
              let len = String.length s in
              if len < 2 then raise Exit;
              if s.[0] <> 'E' then raise Exit;
              let add = match s.[1] with
                | '+' -> false
                | '-' -> true
                | _ -> raise Exit
              in
              let n = int_of_string ( String.sub s 2 (len-2) ) in
              if add then
                silent_errors := IntSet.add n !silent_errors
              else
                silent_errors := IntSet.remove n !silent_errors
            with _ ->
              Printf.eprintf "Error: invalid argument for --error 'E+/-NN'\n%!";
              exit 2),
        EZCMD.info ~docv:"ERROR" "Enable  (use 'E+NN') or disable (use 'E-NN') error NN";

      ]

      ~doc:"Read a TOML file"
      action

  in
  EZCMD.main cmd

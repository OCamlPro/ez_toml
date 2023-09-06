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

open Internal_types

open EzCompat
open Types

let is_table_node node = match node.node_value with
  | Table _ -> true
  | _ -> false

let default_config = {
  allow_override = false ;
  allow_extops = false ;
  pedantic = false ;
  newline = "\n";
}

let rec get_node_table ~loc node =
  match node.node_value with
  | Array array ->
      let len = Array.length array in
      if len = 0 || node.node_format = Inline then
        Internal_misc.error ~loc 3 Invalid_lookup_in_inline_array ;
      get_node_table ~loc array. ( len - 1 )
  | Table table -> node, table
  | _ -> Internal_misc.error ~loc 2 Invalid_lookup

let get_key ~loc node key =
  let _node, table = get_node_table ~loc node in
  StringMap.find key table

let set_key ~loc config node key v =
  let node, table = get_node_table ~loc node in
  if not config.allow_override && StringMap.mem key table then
    Internal_misc.error ~loc 4 ( Key_already_exists [ key ] ) ;
  node.node_value <- Table ( StringMap.add key v table )

let rec get_key_path ~loc node key_path =
  match key_path with
  | [] -> node
  | key :: key_path ->
      let node = get_key ~loc node key in
      get_key_path ~loc node key_path

let rec set_key_path ~loc config table_node key_path ~value:v =
  match key_path with
  | [] -> assert false
  | [ key ] ->
      let table_node, table = get_node_table ~loc table_node in
      if not config.allow_override && StringMap.mem key table then
        Internal_misc.error ~loc 4 ( Key_already_exists key_path ) ;
      table_node.node_value <- Table ( StringMap.add key v table )
  | key :: key_path ->
      let new_table_node = try
          get_key ~loc table_node key
        with Not_found ->
          (*          Printf.eprintf "New table for %s\n%!" key; *)
          let new_node = Internal_misc.node ~loc @@ Table StringMap.empty in
          set_key ~loc config table_node key new_node;
          new_node
      in
      set_key_path ~loc config new_table_node key_path ~value:v

let rec table_mem_key_path ~loc table key_path =
  match key_path with
  | [] -> true
  | key :: key_path ->
      match StringMap.find key table with
      | exception Not_found -> false
      | node ->
          match node.node_value with
          | Table table -> table_mem_key_path ~loc table key_path
          | _ ->
              Internal_misc.error ~loc 5 (Invalid_key_set key)

let val_node v value =
  Internal_misc.node ~loc:v.loc value

let rec node_of_inline config v =
  match v.txt with
  | IBool b -> val_node v @@ Bool b
  | IString (_, s) -> val_node v @@ String s
  | IInt s -> val_node v @@ Int s
  | IFloat s -> val_node v @@ Float s
  | IDate s -> val_node v @@ Date s
  | IArray array ->
      val_node v @@ Array ( Array.of_list array |>
                Array.map (node_of_inline config ))
  | ITable table ->
      let table_node = val_node v @@ Table StringMap.empty in
      List.iter (fun bind ->
          let var = bind.bind_var in
          let loc = var.loc in
          let v = bind.bind_val in
          begin match get_key_path ~loc:var.loc table_node var.txt with
            | exception Not_found -> ()
            | _ -> Internal_misc.error ~loc 4 ( Key_already_exists var.txt );
          end;
          let v = node_of_inline config v in
          set_key_path ~loc:var.loc config table_node var.txt ~value:v
        ) table ;
      table_node

(*
let set_key_path ~loc table_node key_path ~value =
  Printf.eprintf "set_key_path %s\n%!"
    ( string_of_key_path key_path );
  set_key_path ~loc table_node key_path ~value
*)

let line_node line key_path v =
  Internal_misc.node
    ~loc:line.line_operation_loc
    ~before:line.line_comments_before
    ?after:line.line_comment_after
    ~format:Any
    ~name:key_path
    v

let eprint_lines lines =
  List.iter (fun line ->
      match line.line_operation with
      | Array_item key_path ->
          Printf.eprintf "[[ %s ]]\n%!"
            ( Printer.string_of_key_path key_path )
      | Table_item key_path ->
          Printf.eprintf "[ %s ]\n%!" ( Printer.string_of_key_path key_path )
      | Set bind ->
          Printf.eprintf "%s = ...\n%!"
            ( Printer.string_of_key_path bind.bind_var.txt )
      | Error_item error ->
          Printf.eprintf "[!%d]\n%!" error
    ) lines

let table_of_lines config lines =
  let top_node = Internal_misc.node @@ Table StringMap.empty in

  let rec iter ( prefix : key_path ) lines =
    match lines with
    | [] -> ()
    | line :: lines ->
        let loc = line.line_operation_loc in
        match line.line_operation with
        | Array_item key_path ->
            let table_node =
              line_node line key_path @@ Table StringMap.empty in
            let array_node = try get_key_path ~loc top_node key_path with
              | Not_found ->
                  let array_node = line_node line key_path @@ Array [||] in
                  set_key_path ~loc config top_node key_path ~value:array_node;
                  array_node
            in
            begin
              match array_node.node_value with
              | Array old_array ->
                  if not ( Array.for_all is_table_node old_array ) then
                    Internal_misc.error ~loc 8 ( Append_item_to_non_table_array key_path );

                  array_node.node_value <-
                    Array ( Array.concat [ old_array ;
                                           [| table_node |] ] )
              | _ ->
                  Internal_misc.error ~loc 7 ( Append_item_to_non_array key_path )
            end;
            iter key_path lines
        | Table_item [] ->
            iter [] lines
        | Table_item key_path ->
            begin
              match get_key_path ~loc top_node key_path with
              | exception Not_found ->
                  let table_node =
                    line_node line key_path @@ Table StringMap.empty in
                  set_key_path ~loc config top_node key_path ~value:table_node
              | table_node ->
                  match table_node.node_value with
                  | Table _ ->
                      if config.pedantic then
                        Internal_misc.error ~loc 16
                          ( Duplicate_table_item key_path )
                  | _ ->
                      Internal_misc.error ~loc 6 ( Invalid_table key_path )
            end;
            iter key_path lines
        | Set bind ->
            begin
              let var = bind.bind_var in
              let v = bind.bind_val in
              let key_path = prefix @ var.txt in
              let node = line_node line key_path
                  ( node_of_inline config v ).node_value in
              set_key_path ~loc config top_node key_path ~value:node
            end;
            iter prefix lines
        | Error_item n ->
            match lines with
            | [] ->
                Internal_misc.error ~loc 10
                  ( Expected_error_before_end_of_file n )
            | line :: lines ->
                (* eprint_lines [ line ]; *)
                match iter prefix [ line ] with
                | () ->
                    Internal_misc.error ~loc 11
                      ( Expected_error_did_not_happen n )
                | exception Error (loc, nn, error ) ->
                    if n <> nn then
                      Internal_misc.error ~loc 12
                        ( Expected_error_but_another_error (n, loc, nn, error ));
                    iter prefix lines
  in
  iter [] lines;
  match top_node.node_value with
  | Table table -> table
  | _ -> assert false

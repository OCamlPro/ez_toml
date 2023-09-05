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
open Types
open Internal_types
open Lexing

let last_line = ref None
let waiting_loc = ref None (* location of first waiting comment *)
let waiting_comments_rev = ref []

let string_of_key_path key_path = String.concat "." key_path

let init () =
  last_line := None ;
  waiting_loc := None ;
  waiting_comments_rev := []

let loc_of_pos (begin_pos, end_pos) =
  {
    file = begin_pos.pos_fname ;
    line_begin = begin_pos.pos_lnum ;
    char_begin = begin_pos.pos_bol ;
    line_end = end_pos.pos_lnum ;
    char_end = end_pos.pos_bol ;
  }

let loc_of_lexbuf lexbuf =
  loc_of_pos ( lexbuf.lex_start_p, lexbuf.lex_curr_p )

let expand_loc l1 l2 =
  { l1 with
    line_end = l2.line_end ;
    char_end = l2.char_end }

let update_loc lexbuf =
  let pos = lexbuf.lex_curr_p in
  last_line := None ;
  lexbuf.lex_curr_p <- {
    pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }

let line pos  operation =
  let line_comments_before = List.rev !waiting_comments_rev in
  waiting_comments_rev := [] ;
  let line_operation_loc = loc_of_pos pos in
  let line_global_loc = match !waiting_loc with
    | None -> line_operation_loc
    | Some comment_loc ->
        waiting_loc := None ;
        expand_loc comment_loc line_operation_loc
  in
  let line =
    {
      line_comments_before ;
      line_comment_after = None ;
      line_operation = operation ;
      line_global_loc ;
      line_operation_loc ;
    }
  in
  last_line := Some line ;
  line

let allow_override = ref false

let noloc = { file = "";
              line_begin = 0;
              line_end = 0;
              char_begin = 0;
              char_end = 0 }

let new_node ?(pos=max_int) ?(format=Any) ?(loc=noloc)
    ?(before=[]) ?(name=["???"]) ?after
    value =
  { node_loc = loc ;
    node_comment_before = before;
    node_comment_after = after;
    node_value = value ;
    node_format = format ;
    node_pos = pos;
    node_name = name;
  }

let node = new_node

let add_comment lexbuf s =
  let loc = loc_of_lexbuf lexbuf in
  begin
    match !last_line with
    | Some line ->
        line.line_global_loc <- expand_loc line.line_global_loc loc ;
        line.line_comment_after <- Some s
    | None ->
        waiting_comments_rev := s :: !waiting_comments_rev;
        match !waiting_loc with
        | None -> waiting_loc := Some loc
        | Some _ -> ()
  end;
  ()

let is_table_node node = match node.node_value with
  | Table _ -> true
  | _ -> false

let error ?(loc=noloc) error = raise @@ Error ( loc, error )

let rec get_node_table ?loc node =
  match node.node_value with
  | Array array ->
      let len = Array.length array in
      if len = 0 || node.node_format = Inline then
        error ?loc Invalid_lookup_in_inline_array ;
      get_node_table ?loc array. ( len - 1 )
  | Table table -> node, table
  | _ -> error ?loc Invalid_lookup

let get_key ?loc node key =
  let _node, table = get_node_table ?loc node in
  StringMap.find key table

let set_key ?loc node key v =
  let node, table = get_node_table ?loc node in
  node.node_value <- Table ( StringMap.add key v table )

let rec get_key_path ?loc node key_path =
  match key_path with
  | [] -> node
  | key :: key_path ->
      let node = get_key ?loc node key in
      get_key_path ?loc node key_path

let rec set_key_path ?loc table_node key_path ~value:v =
  match key_path with
  | [] -> assert false
  | [ key ] ->
      let table_node, table = get_node_table ?loc table_node in
      if StringMap.mem key table then
        error ?loc ( Key_already_exists key_path ) ;
      table_node.node_value <- Table ( StringMap.add key v table )
  | key :: key_path ->
      let new_table_node = try
          get_key ?loc table_node key
        with Not_found ->
          (*          Printf.eprintf "New table for %s\n%!" key; *)
          let new_node = node @@ Table StringMap.empty in
          set_key table_node key new_node;
          new_node
      in
      set_key_path ?loc new_table_node key_path ~value:v

let rec table_mem_key_path ?loc table key_path =
  match key_path with
  | [] -> true
  | key :: key_path ->
      match StringMap.find key table with
      | exception Not_found -> false
      | node ->
          match node.node_value with
          | Table table -> table_mem_key_path ?loc table key_path
          | _ ->
              error ?loc (Invalid_key_set key)

let rec node_of_inline v =
  match v.txt with
  | IBool b -> node @@ Bool b
  | IString (_, s) -> node @@ String s
  | IInt s -> node @@ Int s
  | IFloat s -> node @@ Float s
  | IDate s -> node @@ Date s
  | IArray array ->
      node @@ Array ( Array.of_list array |>
                Array.map node_of_inline )
  | ITable table ->
      let table_node = node @@ Table StringMap.empty in
      List.iter (fun bind ->
          let var = bind.bind_var in
          let loc = var.loc in
          let v = bind.bind_val in
          begin match get_key_path ~loc:var.loc table_node var.txt with
            | exception Not_found -> ()
            | _ -> error ~loc ( Key_already_exists var.txt );
          end;
          let v = node_of_inline v in
          set_key_path ~loc:var.loc table_node var.txt ~value:v
        ) table ;
      table_node

(*
let set_key_path ?loc table_node key_path ~value =
  Printf.eprintf "set_key_path %s\n%!"
    ( string_of_key_path key_path );
  set_key_path ?loc table_node key_path ~value
*)

let table_of_lines lines =
  let top_node = node @@ Table StringMap.empty in

  let rec iter ( prefix : key_path ) lines =
    match lines with
    | [] -> ()
    | line :: lines ->
        let loc = line.line_operation_loc in
        match line.line_operation with
        | Array_item key_path ->
            let table_node = node @@ Table StringMap.empty in
            let array_node = try get_key_path ~loc top_node key_path with
              | Not_found ->
                  let array_node = node @@ Array [||] in
                  set_key_path ~loc top_node key_path ~value:array_node;
                  array_node
            in
            begin
              match array_node.node_value with
              | Array old_array ->
                  if not ( Array.for_all is_table_node old_array ) then
                    failwith
                      "Appending a table array item to a non-table array";
                  array_node.node_value <-
                    Array ( Array.concat [ old_array ;
                                           [| table_node |] ] )
              | _ ->
                  failwith "Appending a table array item to a non-array"
            end;
            iter key_path lines
        | Table_item key_path ->
            begin
              match get_key_path ~loc top_node key_path with
              | exception Not_found ->
                  let table_node = node @@ Table StringMap.empty in
                  set_key_path ~loc top_node key_path ~value:table_node
              | table_node ->
                  match table_node.node_value with
                  | Table _ -> ()
                  | _ ->
                      error ~loc ( Invalid_table key_path )
            end;
            iter key_path lines
        | Set bind ->
            begin
              let var = bind.bind_var in
              let v = bind.bind_val in
              let key_path = prefix @ var.txt in
              set_key_path ~loc top_node key_path ~value:(node_of_inline v);
            end;
            iter prefix lines
  in
  iter [] lines;
  match top_node.node_value with
  | Table table -> table
  | _ -> assert false

let node = new_node
let loc pos txt = { loc = loc_of_pos pos  ; txt }


let eprint_lines lines =
  List.iter (fun line ->
      match line.line_operation with
      | Array_item key_path ->
          Printf.eprintf "[[ %s ]]\n%!" ( string_of_key_path key_path )
      | Table_item key_path ->
          Printf.eprintf "[ %s ]\n%!" ( string_of_key_path key_path )
      | Set bind ->
          Printf.eprintf "%s = ...\n%!"
            ( string_of_key_path bind.bind_var.txt )
    ) lines

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


(** Bare keys only allow [A-Za-z0-9_-]. *)
let key_is_bare key =
  let len = String.length key in
  let rec iter i key len =
    if i = len then
      true
    else
      match key.[i] with
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '_'
      | '-' -> iter (i+1) key len
      | _ -> false
  in
  iter 0 key len

let bprint_escape_char b char =
  match char with
  | '"' -> Buffer.add_string b "\\\""
  | '\\' -> Buffer.add_string b "\\\\"
  | '\n' -> Buffer.add_string b "\\n"
  | '\t' -> Buffer.add_string b "\\t"
  | _ ->
    let code = Char.code char in
    if code <= 31 then
      Printf.bprintf b "\\u%04x" code
    else
      Buffer.add_char b char

(* Do we need some form of escaping for non-bare keys ? *)
let bprint_key_path b key_path =
  List.iteri (fun i key ->
      if i > 0 then Buffer.add_char b '.' ;
      if key_is_bare key then
        Buffer.add_string b key
      else
      if String.contains key '"' then
        if String.contains key '\'' then
          Printf.kprintf failwith
            "Key %S cannot contain both simple and double quotes" key
        else
          Printf.bprintf b "'%s'" key
      else
        Printf.bprintf b "\"%s\"" key
    ) key_path

type context =
  | InsideTable
  | InsideArray
  | InsideInlineTable

let is_table_node node =
  match node.node_value, node.node_format with
  | Table _, Any -> true
  | _ -> false

let split_table table =
  let sections = ref [] in
  let simple_values = ref [] in
  StringMap.iter (fun key node ->
      let is_section =
        match node.node_value, node.node_format with
        | Table _, Any -> true
        | Array array, ( Any | Multiline )
          when Array.for_all is_table_node array -> true
        | _ -> false
      in
      if is_section then
        sections := ( key, node ) :: !sections
      else
        simple_values := ( key, node ) :: !simple_values
    ) table ;

  let simple_values = List.rev !simple_values in
  let simple_values = List.stable_sort (fun (_,n1) (_,n2) ->
      compare n1.node_pos n2.node_pos) simple_values in
  let sections = List.rev !sections in
  let sections = List.stable_sort (fun (_,n1) (_,n2) ->
      compare n1.node_pos n2.node_pos) sections in
  (simple_values, sections)

(* Here, we assume that x.y.z = v will be parsed by putting all node
   information in the v node, and not intermediary table nodes *)
let rec extract_singleton key_path node =
  match node.node_value with
  | Table table ->
      if StringMap.cardinal table = 1 then
        let (key, node) = StringMap.min_binding table in
        extract_singleton ( key_path @ [ key ] ) node
      else
        ( key_path, node )
  | _ -> ( key_path, node )

let rec bprint_toplevel_table b table =
  bprint_table b [] ( split_table table )

and bprint_table b prefix (simple_values, sections) =

  List.iter (fun (key,node) ->
      let key_path, node = extract_singleton [key] node in
      bprint_comment_before b node ;
      bprint_key_path b key_path ;
      Printf.bprintf b " = ";
      bprint_value b node.node_format InsideTable node.node_value ;
      bprint_comment_after b node ;
    ) simple_values ;

  List.iter (fun (key,node) ->
      match node.node_value with
      | Table table ->
          bprint_comment_before b node ;
          let (simple_values,sections) = split_table table in
          let key_path = prefix @ [key] in
          begin
            match simple_values with
            | [] -> ()
            | _ ->
                Printf.bprintf b "[";
                bprint_key_path b key_path;
                Printf.bprintf b "]";
                bprint_comment_after b node ;
          end;
          bprint_table b key_path (simple_values, sections);
      | Array array ->
          let key_path = prefix @ [ key ] in
          Array.iter (fun node ->
              bprint_comment_before b node ;
              Buffer.add_string b "[[";
              bprint_key_path b key_path ;
              Buffer.add_string b "]]";
              bprint_comment_after b node ;
              let table = match node.node_value with
                | Table table -> table
                | _ -> assert false
              in
              bprint_table b key_path ( split_table table )
            ) array
      | _ -> assert false
    ) sections ;
  ()

and bprint_comment_before b node =
  List.iter (fun line ->
      Printf.bprintf b "#%s\n" line) node.node_comment_before

and bprint_comment_after b node =
  begin match node.node_comment_after with
    | None -> ()
    | Some comment -> Printf.bprintf  b"%s" comment
  end;
  Printf.bprintf b "\n"

and bprint_value b format context value =
  match value with
  | Int int -> Buffer.add_string b int
  | Float float -> Buffer.add_string b float
  | Date date -> Buffer.add_string b date
  | Bool bool -> Printf.bprintf b "%b" bool
  | Table table ->
      Printf.bprintf b "{ ";
      let simple_values = ref [] in
      StringMap.iter (fun key node ->
          (* all values are simple ! *)
          simple_values := ( key, node ) :: !simple_values
        ) table ;
      let simple_values = List.rev !simple_values in
      let simple_values = List.stable_sort (fun (_,n1) (_,n2) ->
          compare n1.node_pos n2.node_pos) simple_values in

      List.iteri (fun i (key,node) ->
          let key_path, node = extract_singleton [key] node in
          if i > 0 then Buffer.add_string b ", ";
          bprint_key_path b key_path ;
          Printf.bprintf b " = ";
          bprint_value b node.node_format InsideInlineTable node.node_value ;
        ) simple_values ;
      Printf.bprintf b " }"
  | Array array ->
      let allow_multiline = match format, context with
        | Any, InsideInlineTable
        | Inline, _
          -> false
        | _ -> true
      in
      Printf.bprintf b "[ ";
      Array.iteri (fun i node ->
          if i > 0 then Buffer.add_string b ", ";
          bprint_value b node.node_format
            (if allow_multiline then
               InsideArray
             else
               InsideInlineTable) node.node_value ;
        ) array ;
      Printf.bprintf b " ]"
  | String string ->
      let has_newline = String.contains string '\n' in
      begin
        match format, context, has_newline with
        | Literal, _, _ (* for now, just use double-quotes. TODO *)

        | _, InsideInlineTable, _
        | _, InsideArray, _
        | _, _, false
        | Inline, _, _
          ->
            Buffer.add_char b '"';
            String.iter (bprint_escape_char b) string;
            Buffer.add_char b '"'
        | Any, InsideTable, true
        | Multiline, InsideTable, true
          ->
            Buffer.add_string b {|"""|};
            Buffer.add_char b '\n';
            let rec iter nquotes value pos len =
              if pos < len then
                let c = value.[pos] in
                match c with
                | '"' ->
                    let nquotes =
                      if nquotes = 2 then begin
                        bprint_escape_char b c;
                        0
                      end else begin
                        bprint_escape_char b '"';
                        nquotes+1
                      end
                    in
                    iter nquotes value (pos+1) len
                | '\n' ->
                    Buffer.add_char b '\n';
                    iter 0 value (pos+1) len
                | c ->
                    bprint_escape_char b c;
                    iter 0 value (pos+1) len
            in
            iter 0 string 0 (String.length string);
            Buffer.add_string b {|"""|}
      end

let string_of_table table =
  let b = Buffer.create 10000 in
  bprint_toplevel_table b table ;
  Buffer.contents b

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

let get node key_path =
  Internal_parsing.get_key_path ~loc:Internal_misc.noloc node key_path

let set ?(config = Internal_misc.default_config) node key_path
    ~value:internal_node =
  Internal_parsing.set_key_path
    ~config
    ~loc:Internal_misc.noloc
    node key_path ~value:internal_node

let string s = Internal_misc.node @@ String s
let bool b = Internal_misc.node @@ Bool b
let int n = Internal_misc.node @@ Int ( string_of_int n )
let float f = Internal_misc.node @@ Float ( string_of_float f )
let date d = Internal_misc.node @@
  Date ( ISO8601.Permissive.string_of_datetimezone (d, 0.) )
let array t = Internal_misc.node @@ Array t
let table t = Internal_misc.node @@ Table t
let table_of_list t = table ( StringMap.of_list t )

let type_of_value value = match value with
  | Table _ -> "Table"
  | Array _ -> "Array"
  | String _ -> "String"
  | Bool _ -> "Bool"
  | Int _ -> "Int"
  | Float _ -> "Float"
  | Date _ -> "Date"

let type_of_node node = type_of_value node.node_value

let error_mismatch node expected =
  Internal_misc.error 17 ( Type_mismatch (node, expected) )

let error_convertion node expected =
  Internal_misc.error 18 ( Bad_convertion (node, expected) )

let extract_bool node =
  match node.node_value with
  | Bool s -> s
  | _ -> error_mismatch node "Bool"

let extract_int node =
  match node.node_value with
  | Int s ->
      ( try int_of_string s with _ -> error_convertion node "Int" )
  | _ -> error_mismatch node "Int"

let extract_float node =
  match node.node_value with
  | Float s ->
      ( try float_of_string s with _ -> error_convertion node "Float" )
  | _ -> error_mismatch node "Float"

let extract_date node =
  match node.node_value with
  | Date s ->
      ( try
          fst (ISO8601.Permissive.datetime_tz ~reqtime:false s)
        with _ -> error_convertion node "Date" )
  | _ -> error_mismatch node "Date"

let extract_table node =
  match node.node_value with
  | Table s -> s
  | _ -> error_mismatch node "Table"

let extract_array node =
  match node.node_value with
  | Array s -> s
  | _ -> error_mismatch node "Array"

let extract_string node =
  match node.node_value with
  | String s -> s
  | _ -> error_mismatch node "String"


let table_iter node f = StringMap.iter f ( extract_table node )
let array_iteri node f = Array.iteri f ( extract_array node )
let array_length node = Array.length ( extract_array node )

let add_comments node comments =
  node.node_comment_before <- node.node_comment_before @ comments

let add_eol_comment node comment =
  node.node_comment_after <- Some comment

let get_value node = node.node_value
let set_value node v = node.node_value <- v

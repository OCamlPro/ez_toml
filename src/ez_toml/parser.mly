%{
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

    (* Directly imported from toml.7.0.1 *)

    open EzCompat
    open Types

    let node = Misc.node

type op =
  | OpInit of value   (* ==, only set if not already set *)
  | OpEqual of value  (* =, set for the first time *)
  | OpSet of value    (* :=, always set *)
  | OpUnset           (* -=, remove value if exists *)

type t = XValue of value
       | XTable of (string, t) Hashtbl.t
       | XTables of ((string, t) Hashtbl.t) list

(* Indicate if the given table is a [Regular] table (i.e. its name
 * is its real name), or if it is a [ArrayElement] (i.e. its name
 * is the name of the array containing the table). *)
type table_tag = Regular | ArrayElement

(* [lookup_table root keys]
 * For a given path [keys], look over [root] and fetch a table.
 * Creates empty subtables all along the way if needed. *)
let lookup_table root keys =
  List.fold_left
    (fun t k ->
       try match Hashtbl.find t k with
         | XTable t   -> t
         | XValue _   -> failwith ("lookup_table.")
         | XTables [] -> let sub = Hashtbl.create 0 in
           Hashtbl.replace t k (XTables [sub]) ;
           sub
         (* Return the last table defined *)
         | XTables t -> List.hd (List.rev t)
       with Not_found -> let sub = Hashtbl.create 0 in
         Hashtbl.add t k (XTable sub) ;
         sub)
    root keys

(* [add_value t k v] add to the value [v], at the key [k],
 * in the table [t]. Fails if a value is already binded to [k]. *)
let add_value t k v =
  match v with
  | OpEqual v ->
     if Hashtbl.mem t k then
       if !Misc.allow_override then
         Hashtbl.remove t k
       else
         Printf.kprintf failwith "add_value failed: key %S already exists" k;

     Hashtbl.add t k (XValue v)
  | OpInit v ->
     if not ( Hashtbl.mem t k ) then Hashtbl.add t k (XValue v)
  | OpSet v ->
     if Hashtbl.mem t k then Hashtbl.remove t k ;
     Hashtbl.add t k (XValue v)
  | OpUnset ->
     if Hashtbl.mem t k then Hashtbl.remove t k

(* [add_to_table root ks kvs] add [kvs] to table found following path
 * [ks] from [root] table.
 * Use it for a table not in array of tables. *)
let add_to_table root ks kvs =
  let t = lookup_table root ks in
  List.iter (fun (k, v) -> add_value t k v) kvs

(* [add_to_nested_table root ks kvs] add [kvs] to nested
 * table found following path [ks] from [root].
 * Use it for a table which is in an array of tables.  *)
let add_to_nested_table root ks kvs =

  (* [ts] are intermediate tables key,
   * [k] is the value key. *)
  let (ts, k) = let rec aux acc = function
      | [ x ]  -> (List.rev acc, x)
      | h :: q -> aux (h :: acc) q
      | []     -> assert false
    in aux [] ks in

  let t = lookup_table root ts in

  (* [insert_in_new_table ts kvs] create a new table, insert
   * [kvs], a (key * value) list into it, and pakc it with
   * tables [ts] into an array of table. *)
  let insert_table ts kvs =
    let t = Hashtbl.create 0 in
    List.iter(fun (k, v) -> add_value t k v) kvs ;
    XTables (ts @ [ t ]) in

  try match Hashtbl.find t k with
    | XTables ts -> Hashtbl.replace t k (insert_table ts kvs);
    | XTable _   -> failwith (
                       Printf.sprintf
                         "%s is a table, not an array of tables" k)
    | XValue _   -> failwith ("add_to_nested_table")
  with Not_found  -> Hashtbl.add t k (insert_table [] kvs)

(* Convert a value of local type [t] into a [Value.value]  *)
let rec convert v =
  let value = match v with
    | XTable t   -> Table (htbl_to_map t)
    | XValue v   -> v
    | XTables ts ->
       Array ( List.filter (fun t -> Hashtbl.length t > 0) ts
               |> List.map (fun htbl -> Misc.node @@ Table (htbl_to_map htbl))
               |> Array.of_list )
  in
  Misc.node value

and htbl_to_map h =
  Hashtbl.fold (fun k v map -> StringMap.add k (convert v) map)
    h StringMap.empty

let to_table key_values key_value_groups =
  let t = Hashtbl.create 0 in
  List.iter (fun ((tag, ks), kvs) ->
      match tag with
      | Regular      -> add_to_table t ks kvs
      | ArrayElement -> add_to_nested_table t ks kvs)
    (* Create a dummy table with empty key for values
     * which are direct children of root table.  *)
    (((Regular, []), key_values) :: key_value_groups) ;
  match ( convert @@ XTable t) . node_value with
    Table t -> t | _ -> assert false

let to_table2 key_values key_value_groups =
  let t = Hashtbl.create 0 in

  let add_to_table t ks v =
    match List.rev ks with
    | [] -> assert false
    | k :: rks ->
       add_to_table t (List.rev rks) [k, v]
  in

  List.iter (fun ((tag, ks), kvs) ->
      match tag with
      | Regular      ->
         List.iter (fun (k, v) -> add_to_table t ( ks @  k ) v) kvs
      | ArrayElement ->
         add_to_nested_table t ks (List.map (function
                                       | [x], v -> x,v
                                       | _ -> assert false) kvs)
    )
    ( ((Regular, []), key_values) :: key_value_groups );
  match ( convert @@ XTable t) . node_value with
    Table t -> t | _ -> assert false
%}

(* OcamlYacc definitions *)
%token <bool> BOOL
%token <string> INTEGER
%token <string> FLOAT
%token <string> STRING
%token <string> DATE
%token <string> KEY
%token LBRACK RBRACK LBRACE RBRACE EOF COMMA DOT
%token EQUAL INIT SET CLEAR

%start toml

%type <Types.table> toml
%type <string * op> keyValue
%type <Types.node list> array_start

%%
(* Grammar rules *)
toml:
 | keysValue* pair( group_header, keysValue* )* EOF
   { to_table2 $1 $2 }

group_header:
 | LBRACK LBRACK key_path RBRACK RBRACK { ArrayElement, $3 }
 | LBRACK key_path RBRACK               { Regular, $2 }

key:
 | STRING { $1 }
 | KEY    { $1 }
 | INTEGER    { $1 }

key_path: k = separated_nonempty_list (DOT, key) { k }

keysValue:
 | key_path EQUAL value { ($1, OpEqual $3) }
 | key_path INIT value { ($1, OpInit $3) }
 | key_path SET value { ($1, OpSet $3) }
 | key_path CLEAR { ($1, OpUnset) }

keyValue:
 | key EQUAL value { ($1, OpEqual $3) }
 | key INIT value { ($1, OpInit $3) }
 | key SET value { ($1, OpSet $3) }
 | key CLEAR { ($1, OpUnset) }

value:
    BOOL { Bool($1) }
  | INTEGER { Int $1 }
  | FLOAT { Float $1 }
  | STRING { String($1) }
  | DATE { Date $1 (* (fst (ISO8601.Permissive.datetime_tz ~reqtime:false date)) } *)}
  | LBRACK array_start { Array( Array.of_list $2) }
  | inline_table { Table($1) }

inline_table:
  LBRACE; key_values = inline_table_key_values; RBRACE {
  to_table key_values [] }

inline_table_key_values:
  key_values = separated_list(COMMA, keyValue) { key_values }

array_start:
    COMMA value array_start { node $2 :: $3 }
  | COMMA? RBRACK { [] }

%%

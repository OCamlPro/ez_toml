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

open Ez_toml.V1
(* open TOML.Types *)

let initial_file =
  {|

a = "1"

[[b.item]]

value = 1

[[b.item]]

name = "second item"
value = 2

[c.d.e]

f = [ 1 , 2]

|}

let initial_output = {|

a = "1"

[[b.item]]

value = 1

[[b.item]]

name = "second item"
value = 2

[c.d.e]

f = [ 1, 2 ]
|}

let modified_output = {|

a = "1"
ab = 2

[[b.item]]

value = 1

[[b.item]]

name = "second item"
value = 2
[c]
g = "bonjour"

[c.d.e]

f = [ 1, 2 ]
[l]
ab = 2
|}


let check_to_string msg t expected_string =
  let s = TOML.to_string t in
  if s <> expected_string then begin
    Printf.eprintf "Error: unexpected output with %S:\n%!" msg;
    Printf.eprintf "expected_output = {|%s|}\n%!" expected_string;
    Printf.eprintf "\n==========================================\n\n";
    Printf.eprintf "obtained output = {|%s|}\n%!" s;
    exit 2
  end

let run_and_exit () =

  let t = TOML.of_string initial_file in

  check_to_string "initial" t initial_output ;

  assert ( TOML.get t [ "a" ] |> TOML.extract_string = "1" );
  let items =  TOML.get t [ "b" ; "item" ] |> TOML.extract_array in
  assert ( Array.length items = 2 );
  assert ( TOML.get items.(0) [ "value" ] |> TOML.extract_int = 1 );

  assert ( TOML.get items.(1) [ "value" ] |> TOML.extract_int = 2 );
  assert ( TOML.get items.(1) [ "name" ]
           |> TOML.extract_string = "second item" );

  let f = TOML.get t [ "c" ; "d" ; "e" ; "f" ] |> TOML.extract_array in

  assert ( Array.length f = 2 );
  assert ( TOML.extract_int f.(0) = 1 );
  assert ( TOML.extract_int f.(1) = 2 );

  let t2 = TOML.update t [ "c" ; "g" ] (Some (TOML.string "bonjour")) in
  let t2 = TOML.update t2 [ "aa" ] (Some (TOML.int 1)) in
  let t2 = TOML.update t2 [ "ab" ] (Some (TOML.int 2)) in
  let t2 = TOML.update t2 [ "aa" ] None in
  let t2 = TOML.update t2 [ "l" ; "aa" ] (Some (TOML.int 1)) in
  let t2 = TOML.update t2 [ "l" ; "ab" ] (Some (TOML.int 2)) in
  let t2 = TOML.update t2 [ "l" ; "aa" ] None in

  check_to_string "not muted" t initial_output ;

  check_to_string "functional update" t2 modified_output ;

  TOML.set t [ "c" ; "g" ] ~value:(TOML.string "bonjour") ;
  TOML.set t [ "aa" ] ~value:(TOML.int 1) ;
  TOML.set t [ "ab" ] ~value: (TOML.int 2) ;
  TOML.remove t [ "aa" ] ;
  TOML.set t [ "l" ; "aa" ] ~value:(TOML.int 1) ;
  TOML.set t [ "l" ; "ab" ] ~value:(TOML.int 2) ;
  TOML.remove t [ "l" ; "aa" ] ;

  check_to_string "muted" t modified_output ;

  exit 0

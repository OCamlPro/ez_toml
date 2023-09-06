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

open Types

let noloc = { file = "";
              line_begin = 0;
              line_end = 0;
              char_begin = 0;
              char_end = 0 }

let error ?(loc=noloc) n error = raise @@ Error ( loc, n, error )

let default_config = {
  allow_override = false ;
  allow_extops = false ;
  pedantic = false ;
  newline = "\n";
}

let node_counter = ref 0
let node ?(format=Any) ?(loc=noloc)
    ?(before=[]) ?(name=["???"]) ?after
    value =
  incr node_counter;
  { node_loc = loc ;
    node_comment_before = before;
    node_comment_after = after;
    node_value = value ;
    node_format = format ;
    node_pos = !node_counter;
    node_name = name;
  }

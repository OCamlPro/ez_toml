
#directory "_opam/lib/ocplib_stuff";;
#directory "_opam/lib/ISO8601";;
#directory "_opam/lib/re";;
#directory "_opam/lib/re/glob";;
#directory "_opam/lib/ez_file";;
#directory "_build/install/default/lib/ez_toml";;
(* #directory "_build/default/src/ez_toml";;  *)

#load "unix.cma";;
#load "ocplib_stuff.cma";;
#load "ISO8601.cma";;
#load "re.cma";;
#load "re_glob.cma";;
#load "ez_file.cma";;
#load "ez_toml.cma";;


open Ez_toml.V1

open TOML.Types

(* Look up a deeply nested value with a known type. *)
let t = TOML.of_string "
[settings]
  [settings.basic]
    crash_randomly = true
" ;;

(*
  val t : Ez_toml.V1.TOML.Types.node =
  {node_comment_before = []; node_comment_after = None;
   node_loc =
    {file = ""; line_begin = 2; line_end = 5; char_begin = 0; char_end = 0};
   node_format = Any; node_pos = 1; node_value = Table <abstr>;
   node_name = ["???"]}
*)

(* Look up a deeply nested value with a known type. *)
let crash_randomly =
  TOML.get t  ["settings"; "basic"; "crash_randomly"]
  |> TOML.extract_bool

(*  val crash_randomly : bool = true *)

(* Update a deeply nested value. *)
(* This would fail because the key already exists:

let () =
  TOML.set t ["settings"; "basic"; "crash_randomly"] (TOML.int 0)
;;

raises exception 4, Ez_toml.Types.Key_already_exists ["crash_randomly"] *)

(* so, We need to silent overriding errors *)
let config = { TOML.default_config with
	       silent_errors =
	         EzCompat.IntSet.add 4 TOML.default_config.silent_errors }
;;

let () =
  TOML.set ~config t ["settings"; "basic"; "crash_randomly"]
     ~value:(TOML.int 0)
;;

let () =
  TOML.set ~config t ["other"; "counters"; "recorded_crashes"]
     ~value:(TOML.int 0)
;;

(* Print it back to a string *)
let s = TOML.to_string t
;;

(* val s : string = "\n[settings.basic]\ncrash_randomly = 0\n" *)

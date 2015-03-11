(*s: ./commons/log.ml *)
open Printf

(*s: constant Log.debug_mode *)
let debug_mode = ref true
(*e: constant Log.debug_mode *)

(*s: function Log.f *)
(* flushes ! *)
let f s = 
  try prerr_endline s 
  with _ -> ()
(*e: function Log.f *)

(*s: function Log.debug *)
let debug s = 
  if !debug_mode 
  then f (s ^ "\n")
(*e: function Log.debug *)

(*e: ./commons/log.ml *)

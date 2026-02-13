(*s: ./commons/condition.ml *)
(* Conditions *)
open Common

(*s: type Condition.t *)
type t = string
(*e: type Condition.t *)

type condition_backend = {
  create: t -> unit;
  set: t -> unit;
  wait: t -> unit;
  free: t -> unit;
}

let default_backend () = {
  create = (fun _s -> ());
  set = (fun _s -> ());
  wait = (fun _s -> ());
  free = (fun _s -> ());
}
let backend = ref (default_backend ())
  
let count = ref 0

let create () =
  incr count;
  let var = spf "var%d" !count in
  (!backend).create var;
  var

let set s =
  !backend.set s

let wait s = 
  !backend.wait s

let free s = 
  !backend.free s
(*e: ./commons/condition.ml *)

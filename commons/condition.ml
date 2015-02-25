(*s: ./commons/condition.ml *)
(* Conditions *)
(* We don't take any chance with the semantics of tkwait variable, in the
   sense that we make sure that the value changes each time we set the
   condition
 *)

open Printf

(*s: type Condition.t *)
type t = Textvariable.textVariable * int ref
(*e: type Condition.t *)

let create () =
  Textvariable.create(), ref 0

let set (v , r) =
  incr r; Textvariable.set v (sprintf "cond%d" !r)

let wait (v, _) = Tkwait.variable v

let free (v, _) = Textvariable.free v
(*e: ./commons/condition.ml *)

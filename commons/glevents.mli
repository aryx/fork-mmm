(*s: ./commons/glevents.mli *)
open Tk

(*s: signature Glevents.get *)
val get : string -> (modifier list * xEvent) list 
(*e: signature Glevents.get *)
(*s: signature Glevents.reset *)
val reset : unit -> unit
(*e: signature Glevents.reset *)
(*e: ./commons/glevents.mli *)

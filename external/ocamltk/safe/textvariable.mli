(* Support for Tk -textvariable option *)
module Textvariable : sig
open Widget
type textVariable
      (* TextVariable is an abstract type *)

val create : unit -> textVariable
      (* Allocation of a textVariable *)
val create_temporary : widget -> textVariable
      (* Allocation of a textVariable with lifetime associated to widget *)
val set : textVariable -> string -> unit
      (* Setting the val of a textVariable *)
val get : textVariable -> string
      (* Reading the val of a textVariable *)
val name : textVariable -> string
      (* Its tcl name *)

end

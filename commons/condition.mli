(*s: ./commons/condition.mli *)
type t

(*s: signature Condition.create *)
val create : unit -> t
(*e: signature Condition.create *)
(*s: signature Condition.set *)
val set : t -> unit
(*e: signature Condition.set *)
(*s: signature Condition.wait *)
val wait : t -> unit
(*e: signature Condition.wait *)
(*s: signature Condition.free *)
val free : t -> unit
(*e: signature Condition.free *)
(*e: ./commons/condition.mli *)

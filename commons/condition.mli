(*s: ./commons/condition.mli *)
type t

type condition_backend = {
  create: t -> unit;
  set: t -> unit;
  wait: t -> unit;
  free: t -> unit;
}

val backend: condition_backend ref

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

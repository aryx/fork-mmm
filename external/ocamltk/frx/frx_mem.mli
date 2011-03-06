(* A Garbage Collector Gauge for Caml *)

val init : unit -> unit
  (* [init ()] creates the gauge and its updater, but keeps it iconified *)

val f : unit -> unit
  (* [f ()] makes the gauge visible if it has not been destroyed *)

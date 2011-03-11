(* An abstract notion of connection *)

type internal = Unix.file_descr

type t = {
  feed_read : string -> int -> int -> int;
  feed_schedule : (unit -> unit) -> unit;
  feed_unschedule : unit -> unit;
  feed_close : unit -> unit;
  feed_internal : internal  
  }

val of_fd : Unix.file_descr -> t
val internal : t -> internal

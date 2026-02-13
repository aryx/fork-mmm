(*s: ./commons/feed.mli *)
(*s: type Feed.internal *)
(* An abstract notion of connection *)
type internal = Unix.file_descr
(*e: type Feed.internal *)

(*s: type Feed.t *)
type t = {
  feed_read : bytes -> int -> int -> int;

  feed_schedule : (unit -> unit) -> unit;
  feed_unschedule : unit -> unit;

  feed_close : unit -> unit;

  feed_internal : internal  
}
(*e: type Feed.t *)

(*s: signature Feed.of_fd *)
val of_fd : Unix.file_descr -> t
(*e: signature Feed.of_fd *)
(*s: signature Feed.internal *)
val internal : t -> internal
(*e: signature Feed.internal *)
(*e: ./commons/feed.mli *)

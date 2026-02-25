(*s: commons/feed.mli *)

(*s: type [[Feed.internal]] *)
(* An abstract notion of connection *)
type internal = Unix.file_descr
(*e: type [[Feed.internal]] *)

(*s: type [[Feed.t]] *)
type t = {
  feed_read : bytes -> int -> int -> int;

  feed_schedule : (unit -> unit) -> unit;
  (* for abort? *)
  feed_unschedule : unit -> unit;

  feed_close : unit -> unit;

  (* for ?? *)
  feed_internal : internal  
}
(*e: type [[Feed.t]] *)

(*s: signature [[Feed.of_fd]] *)
val make_feed : Unix.file_descr -> (bytes -> int -> int -> int) -> t
(*e: signature [[Feed.of_fd]] *)
(*s: signature [[Feed.internal]] *)
val internal : t -> internal
(*e: signature [[Feed.internal]] *)
(*e: commons/feed.mli *)

(*s: gui/history.mli *)

(*s: type [[History.history_entry]] *)
(* 
   Linear history: we keep going adding to the end of the list,
   EXCEPT when you go back and then on a new link.
*)
type entry = {
  h_did : Document.id;
  h_fragment : string option;

  h_prev : entry option;
  mutable h_next : entry option
  }
(*e: type [[History.history_entry]] *)

(*s: type [[History.t]] *)
type t = {
  mutable h_start : entry;
  mutable h_current: entry;

  h_key : int;
  mutable h_first : bool
  }
(*e: type [[History.t]] *)

val create: Document.id -> t

val add: t -> Document.id -> string option -> unit

val back: t -> (Document.id * string option) option
val forward: t -> (Document.id * string option) option

val contents: t -> entry list

val set_current: t -> entry -> unit
(*e: gui/history.mli *)

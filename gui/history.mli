(*s: gui/history.mli *)

(*s: type [[History.history_entry]] *)
(* 
   Linear history: we keep going adding to the end of the list,
   EXCEPT when you go back and then on a new link.
*)
type history_entry = {
  h_did : Document.document_id;
  h_fragment : string option;

  h_prev : history_entry option;
  mutable h_next : history_entry option
  }
(*e: type [[History.history_entry]] *)

(*s: type [[History.t]] *)
type t = {
  mutable h_start : history_entry;
  mutable h_current: history_entry;

  h_key : int;
  mutable h_first : bool
  }
(*e: type [[History.t]] *)

val create: Document.document_id -> t

val add: t -> Document.document_id -> string option -> unit

val back: t -> (Document.document_id * string option) option
val forward: t -> (Document.document_id * string option) option

val contents: t -> history_entry list

val set_current: t -> history_entry -> unit

(*e: gui/history.mli *)

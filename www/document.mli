(* Document Id is a reference to a document in the browser
   For some documents, e.g. results of POST queries, the URL is not a
   sufficient description. Stamp is 0 for unique documents.
*)
type document_id = {
  document_url : Url.t;
  document_stamp : int
  }

val no_stamp : int
val new_stamp : unit -> int

type logger
val tty_logger : logger

(* This is passed around by request continuations. It represents a handle
   on a connexion for retrieving a document *)
type handle = {
  document_id : document_id;
  document_referer : string option;
    (* URL of refering document, if any *)
  mutable document_status : int;
    (* Status code of response *)
  mutable document_headers : string list;
    (* HTTP headers of document, or faked ones *)
  document_feed : Feed.t;
    (* where to get the data *)
  document_fragment : string option;
    (* fragment (#foo) if any *)
  mutable document_logger : logger
    (* how to log information relative to this document processing *)
}

type document_continuation = {
  document_process : handle -> unit;
    (* What to do one we have a dh on the real document *)
  document_finish :  bool -> unit
    (* What to do if a request does not yield a document *)
}

(*
 * Information on a document, as could be requested by "other" clients,
 * that is clients not directly on the chain of processes dealing with
 * the handle
 *)

type document_data =
   MemoryData of Ebuffer.t
 | FileData of string * bool (* flag is true if file is temporary *)

type document = {
  document_address : Url.t;
  mutable document_data : document_data;
  document_info : string list
  }

module DocumentIDSet : Set.S with type elt = document_id

val dclose : bool -> handle -> unit
  (* [dclose remactive dh] closes a living dh *)

val add_log: handle -> string -> (unit -> unit) -> unit
val put_log : handle -> string -> unit
val progress_log : handle -> int -> unit
val end_log : handle -> string -> unit
val destroy_log : handle -> bool -> unit
  (* logging functions *)

val document_id : Www.request -> document_id

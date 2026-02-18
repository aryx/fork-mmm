(*s: www/document.mli *)
(*s: type [[Document.document_id]] *)
(* Document Id is a reference to a document in the browser.
   For some documents, e.g. results of POST queries, the URL is not a
   sufficient description. Stamp is 0 for unique documents.
*)
type id = {
  document_url : Url.t;
  document_stamp : int
}
(*e: type [[Document.document_id]] *)

(*s: signature [[Document.no_stamp]] *)
val no_stamp : int
(*e: signature [[Document.no_stamp]] *)
(*s: signature [[Document.new_stamp]] *)
val new_stamp : unit -> int
(*e: signature [[Document.new_stamp]] *)

(*s: signature type Document.logger *)
(* pad: exported for tk_document, but normally should be abstract *)
type logger = {
  logger_destroy : bool -> unit;
  logger_progress : int -> unit;
  logger_msg : string -> unit;
  logger_end : string -> unit
}
(*e: signature type Document.logger *)
(*s: signature [[Document.tty_logger]] *)
val tty_logger : logger
(*e: signature [[Document.tty_logger]] *)

(*s: type [[Document.handle]] *)
(* This is passed around by request continuations. It represents a handle
   on a connexion for retrieving a document *)
type handle = {
  document_id : id;

  (* this should help to know what to do even if have not the data yet *)
  mutable dh_headers : string list;
    (* HTTP headers of document, or faked ones *)

  document_feed : Feed.t;
    (* where to get the data *)

  (*s: [[Document.handle]] other fields *)
  mutable document_status : int;
    (* Status code of response *)
  (*x: [[Document.handle]] other fields *)
  document_fragment : string option;
    (* fragment (#foo) if any *)
  (*x: [[Document.handle]] other fields *)
  document_referer: string option;
    (* URL of refering document, if any *)
  (*x: [[Document.handle]] other fields *)
  mutable document_logger : logger;
    (* how to log information relative to this document processing *)
  (*e: [[Document.handle]] other fields *)
}
(*e: type [[Document.handle]] *)

(*s: type [[Document.document_continuation]] *)
type continuation = {
  document_process : handle -> unit;
    (* What to do one we have a dh on the real document *)

  document_finish :  bool -> unit
    (* What to do if a request does not yield a document *)
}
(*e: type [[Document.document_continuation]] *)

(*s: type [[Document.document_data]] *)
(*
 * Information on a document, as could be requested by "other" clients,
 * that is clients not directly on the chain of processes dealing with
 * the handle
 *)
type data =
 | MemoryData of Ebuffer.t
 | FileData of string * bool (* flag is true if file is temporary *)
(*e: type [[Document.document_data]] *)

(*s: type [[Document.document]] *)
type t = {
  document_headers : string list; (* e.g. Content-type: text/html *)
  mutable document_data : data;

  document_address : Url.t; (* origin? *)
}
(*e: type [[Document.document]] *)

module DocumentIDSet : Set.S with type elt = id

(*s: signature [[Document.dclose]] *)
val dclose : bool -> handle -> unit
  (* [dclose remactive dh] closes a living dh *)
(*e: signature [[Document.dclose]] *)

val add_log_backend: (handle -> string -> (unit -> unit) -> unit) ref

(*s: signature [[Document.add_log]] *)
val add_log: handle -> string -> Www.aborter -> unit
(*e: signature [[Document.add_log]] *)
(*s: signature [[Document.put_log]] *)
val put_log : handle -> string -> unit
(*e: signature [[Document.put_log]] *)
(*s: signature [[Document.progress_log]] *)
val progress_log : handle -> int -> unit
(*e: signature [[Document.progress_log]] *)
(*s: signature [[Document.end_log]] *)
val end_log : handle -> string -> unit
(*e: signature [[Document.end_log]] *)
(*s: signature [[Document.destroy_log]] *)
val destroy_log : handle -> bool -> unit
  (* logging functions *)
(*e: signature [[Document.destroy_log]] *)

(*s: signature [[Document.document_id]] *)
val document_id : Www.request -> id
(*e: signature [[Document.document_id]] *)
(*e: www/document.mli *)

(*s: ./www/document.ml *)
open Feed
open Www
open Hyper

(*s: type Document.logger *)
type logger = {
  logger_destroy : bool -> unit;
  logger_progress : int -> unit;
  logger_msg : string -> unit;
  logger_end : string -> unit
}
(*e: type Document.logger *)

(*s: type Document.document_id *)
(* Document Id is a reference to a document in the browser.
   For some documents, e.g. results of POST queries, the URL is not a
   sufficient description. Stamp is 0 for unique documents.
*)
type document_id = {
  document_url : Url.t;
  document_stamp : int
}
(*e: type Document.document_id *)

(*s: module Document.DocumentIDSet *)
module DocumentIDSet =
  Set.Make(struct type t = document_id let compare = compare end)
(*e: module Document.DocumentIDSet *)

(*s: type Document.handle *)
(* This is passed around by request continuations. It represents a handle
   on a connexion for retrieving a document *)
type handle = {
  document_id : document_id;

  (* this should help to know what to do even if have not the data yet *)
  mutable document_headers : string list;
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
(*e: type Document.handle *)

(*s: type Document.document_continuation *)
type document_continuation = {
  document_process : handle -> unit;
    (* What to do one we have a dh on the real document *)
  document_finish :  bool -> unit
    (* What to do if a request does not yield a document *)
}
(*e: type Document.document_continuation *)

(*s: type Document.document_data *)
(*
 * Information on a document, as could be requested by "other" clients,
 * that is clients not directly on the chain of processes dealing with
 * the handle
 *)

type document_data =
 | MemoryData of Ebuffer.t
 | FileData of string * bool (* flag is true if file is temporary *)
(*e: type Document.document_data *)

(*s: type Document.document *)
type document = {
  mutable document_data : document_data;

  document_address : Url.t; (* origin? *)
  document_info : string list
}
(*e: type Document.document *)

(*s: constant Document.stamp_counter *)
let stamp_counter = ref 0
(*e: constant Document.stamp_counter *)
(*s: constant Document.no_stamp *)
let no_stamp = 0
(*e: constant Document.no_stamp *)

(*s: function Document.new_stamp *)
let new_stamp () =
  incr stamp_counter; !stamp_counter
(*e: function Document.new_stamp *)

(*s: function Document.document_id *)
let document_id wwwr =
  match wwwr.www_link.h_method with
  | POST _  ->
         { document_url = wwwr.www_url; document_stamp = new_stamp()}
  | _ -> { document_url = wwwr.www_url; document_stamp = no_stamp}
(*e: function Document.document_id *)


(*s: function Document.dclose *)
(* Close a connexion. Should be called only by a fileinput callback
      or by somebody attempting to abort the connexion 
   We remove the fd of the select before closing it since we don't want
   a spurious read to happen. This way we are somewhat independant of the
   Tk implementation 
 *)

let dclose remactive dh =
  dh.document_feed.feed_unschedule();
  dh.document_feed.feed_close();
  if remactive then Www.rem_active_cnx dh.document_id.document_url
(*e: function Document.dclose *)


(*s: constant Document.tty_logger *)
let tty_logger = 
  { logger_destroy = (fun _ -> ());
    logger_progress = (fun _ -> ());
    logger_msg = Log.f;
    logger_end = Log.f
   }
(*e: constant Document.tty_logger *)

let add_log_backend = ref (fun _ _ _ -> failwith "no add_log defined")
  
(*s: function Document.add_log *)
let add_log dh initmsg aborter =
  !add_log_backend dh initmsg aborter
(*e: function Document.add_log *)
(*s: functions Document.xxx_log *)
let put_log dh = dh.document_logger.logger_msg
let destroy_log dh = dh.document_logger.logger_destroy
let progress_log dh = dh.document_logger.logger_progress
(*e: functions Document.xxx_log *)
(*s: function Document.end_log *)
let end_log dh msg =
    dh.document_logger.logger_end msg;
    destroy_log dh true
(*e: function Document.end_log *)


(*s: type Document.display_info *)
type display_info = {
    di_abort : Www.aborter;
    di_destroy : unit -> unit;
    di_fragment : string option -> unit;
    di_redisplay: unit -> unit;
    di_title : unit -> string;	      (* some title for bookmarks *)
    di_source : unit -> unit;
    di_load_images : unit -> unit
}
(*e: type Document.display_info *)

(*e: ./www/document.ml *)

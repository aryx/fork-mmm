(* Document and image cache *)
open Document

(* Configurable settings *)
val debug : bool ref
val history_mode : bool ref
val max_documents : int ref
val cleann : int ref

val init : unit -> unit

val add : document_id -> document -> unit
val find : document_id -> document
val finished : document_id -> unit
val touch : document_id -> unit
val kill : document_id -> unit

val postmortem : unit -> unit

type cache_fill = {
  cache_write : string -> int -> int -> unit;
  cache_close : unit -> unit
 }

exception DontCache

val tofile : handle -> document_data * cache_fill
val tobuffer: handle -> document_data * cache_fill
val dummy : handle  -> document_data * cache_fill

val discard: cache_fill
val wrap: cache_fill -> handle -> handle

val patch : document_id -> string list -> unit

val cutlinks : (document_id -> unit) list ref

val make_handle : Www.request -> document -> handle
val renew_handle : handle -> handle
val make_embed_handle : document -> handle

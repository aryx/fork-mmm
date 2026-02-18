(*s: protocols/cache.mli *)
(* Document and image cache *)

(*s: signature [[Cache.debug]] *)
(* Configurable settings *)
val debug : bool ref
(*e: signature [[Cache.debug]] *)
(*s: signature [[Cache.history_mode]] *)
val history_mode : bool ref
(*e: signature [[Cache.history_mode]] *)
(*s: signature [[Cache.max_documents]] *)
val max_documents : int ref
(*e: signature [[Cache.max_documents]] *)
(*s: signature [[Cache.cleann]] *)
val cleann : int ref
(*e: signature [[Cache.cleann]] *)

(*s: signature [[Cache.init]] *)
val init : unit -> unit
(*e: signature [[Cache.init]] *)

(*s: signature [[Cache.add]] *)
val add : Document.id -> Document.document -> unit
(*e: signature [[Cache.add]] *)
(*s: signature [[Cache.find]] *)
val find : Document.id -> Document.document
(*e: signature [[Cache.find]] *)
(*s: signature [[Cache.finished]] *)
val finished : Document.id -> unit
(*e: signature [[Cache.finished]] *)
(*s: signature [[Cache.touch]] *)
val touch : Document.id -> unit
(*e: signature [[Cache.touch]] *)
(*s: signature [[Cache.kill]] *)
val kill : Document.id -> unit
(*e: signature [[Cache.kill]] *)

(*s: signature [[Cache.postmortem]] *)
val postmortem : unit -> unit
(*e: signature [[Cache.postmortem]] *)

(*s: type [[Cache.cache_fill]] *)
type cache_fill = {
  cache_write : string -> int -> int -> unit;
  cache_close : unit -> unit
 }
(*e: type [[Cache.cache_fill]] *)

(*s: exception [[Cache.DontCache]] *)
exception DontCache
(*e: exception [[Cache.DontCache]] *)

(*s: signature [[Cache.tofile]] *)
val tofile : Document.handle -> Document.document_data * cache_fill
(*e: signature [[Cache.tofile]] *)
(*s: signature [[Cache.tobuffer]] *)
val tobuffer: Document.handle -> Document.document_data * cache_fill
(*e: signature [[Cache.tobuffer]] *)
(*s: signature [[Cache.dummy]] *)
val dummy : Document.handle  -> Document.document_data * cache_fill
(*e: signature [[Cache.dummy]] *)

(*s: signature [[Cache.discard]] *)
val discard: cache_fill
(*e: signature [[Cache.discard]] *)
(*s: signature [[Cache.wrap]] *)
val wrap: cache_fill -> Document.handle -> Document.handle
(*e: signature [[Cache.wrap]] *)

(*s: signature [[Cache.patch]] *)
val patch : Document.id -> string list -> unit
(*e: signature [[Cache.patch]] *)

(*s: signature [[Cache.cutlinks]] *)
val cutlinks : (Document.id -> unit) list ref
(*e: signature [[Cache.cutlinks]] *)

(*s: signature [[Cache.make_handle]] *)
val make_handle : Www.request -> Document.document -> Document.handle
(*e: signature [[Cache.make_handle]] *)
(*s: signature [[Cache.renew_handle]] *)
val renew_handle : Document.handle -> Document.handle
(*e: signature [[Cache.renew_handle]] *)
(*s: signature [[Cache.make_embed_handle]] *)
val make_embed_handle : Document.document -> Document.handle
(*e: signature [[Cache.make_embed_handle]] *)
(*e: protocols/cache.mli *)

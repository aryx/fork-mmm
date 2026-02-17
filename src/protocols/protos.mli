(*s: protocols/protos.mli *)

(*s: signature [[Protos.get]] *)
val get: Url.protocol ->
  (Www.request -> Document.document_continuation -> Www.aborter) 
  *
  (Document.handle -> Document.document_data * Cache.cache_fill)
(*e: signature [[Protos.get]] *)
(*e: protocols/protos.mli *)

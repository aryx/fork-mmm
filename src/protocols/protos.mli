(*s: protocols/protos.mli *)

(*s: signature [[Protos.get]] *)
val get: Url.protocol ->
  (Www.request -> Document.continuation -> Www.aborter) 
  *
  (Document.handle -> Document.data * Cache.cache_fill)
(*e: signature [[Protos.get]] *)
(*e: protocols/protos.mli *)

(*s: ./http/retype.mli *)
(*s: signature Retype.f *)
val f : Document.handle -> unit
  (* physically modify the headers, adding ContentType/ContentEncoding
   * from URL suffixes if this information is missing from the headers.
   *)
(*e: signature Retype.f *)

(*e: ./http/retype.mli *)

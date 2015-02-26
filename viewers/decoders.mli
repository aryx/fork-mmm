(*s: ./viewers/decoders.mli *)
(* Decoders *)
open Document

(*s: signature Decoders.insert *)
val insert  : Document.handle -> Document.handle
(*e: signature Decoders.insert *)
(*s: signature Decoders.add *)
val add : string -> (Document.handle -> Document.handle) -> unit
(*e: signature Decoders.add *)

(*e: ./viewers/decoders.mli *)

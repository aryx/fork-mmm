(*s: ./viewers/decoders.mli *)
(* Decoders *)
open Document

(*s: signature Decoders.insert *)
val insert  : handle -> handle
(*e: signature Decoders.insert *)
(*s: signature Decoders.add *)
val add : string -> (handle -> handle) -> unit
(*e: signature Decoders.add *)

(*e: ./viewers/decoders.mli *)

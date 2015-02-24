(*s: ./retrieve/retrieve.mli *)
(* Document retrieval *)
open Document

(*s: enum Retrieve.retrievalStatus *)
type retrievalStatus =
  Started of (unit -> unit)  | InUse
(*e: enum Retrieve.retrievalStatus *)

(*s: signature Retrieve.f *)
(* f is supposed to raise only Invalid_url *)
val f : Www.request ->  (* the request *)
        (Hyper.link -> unit) -> (* the retry function *)
        document_continuation -> (* the handlers *)
      	 retrievalStatus
(*e: signature Retrieve.f *)

(*s: enum Retrieve.behaviour *)
type behaviour =
   Ok 
 | Stop of string
 | Retry of Hyper.link
 | Error of string
 | Restart of (handle -> handle)
(*e: enum Retrieve.behaviour *)

(*s: signature Retrieve.add_http_processor *)
val add_http_processor : 
  int -> (Www.request -> handle -> behaviour) -> unit
(*e: signature Retrieve.add_http_processor *)
(*e: ./retrieve/retrieve.mli *)

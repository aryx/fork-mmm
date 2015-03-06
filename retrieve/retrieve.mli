(*s: ./retrieve/retrieve.mli *)
(* Document retrieval *)
open Document

(*s: type Retrieve.retrievalStatus *)
type retrievalStatus =
 | Started of Www.aborter
 | InUse
(*e: type Retrieve.retrievalStatus *)

(*s: signature Retrieve.f *)
(* f is supposed to raise only Invalid_url *)
val f : Www.request ->  (* the request *)
        (Hyper.link -> unit) -> (* the retry function *)
        Document.document_continuation -> (* the handlers *)
        retrievalStatus
(*e: signature Retrieve.f *)

(*s: type Retrieve.behaviour *)
(* We should implement the proper behaviours for all return codes
 * defined in the HTTP/1.0 protocol draft. 
 * Return codes are HTTP specific, but since all protocols are more or
 * less mapped to http, we deal with them at the retrieval level.
 *)
type behaviour =
   Ok 			              (* process the document *)
 | Stop of string             (* stop (no document) and display message *)
 | Retry of Hyper.link        (* restart with a new link *)
 | Error of string            (* same as stop, but it's an error *)
 | Restart of (handle -> handle) (* restart the same request, but apply
                     transformation on the continuation *)
(*e: type Retrieve.behaviour *)

(*s: signature Retrieve.add_http_processor *)
val add_http_processor : 
  int -> (Www.request -> Document.handle -> behaviour) -> unit
(*e: signature Retrieve.add_http_processor *)
(*e: ./retrieve/retrieve.mli *)

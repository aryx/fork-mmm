(* Document retrieval *)
open Document

type retrievalStatus =
  Started of (unit -> unit)  | InUse

(* f is supposed to raise only Invalid_url *)
val f : Www.request ->  (* the request *)
        (Hyper.link -> unit) -> (* the retry function *)
        document_continuation -> (* the handlers *)
      	 retrievalStatus

type behaviour =
   Ok 
 | Stop of string
 | Retry of Hyper.link
 | Error of string
 | Restart of (handle -> handle)

val add_http_processor : 
  int -> (Www.request -> handle -> behaviour) -> unit

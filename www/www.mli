(*
 * Requests
 *)

type request =  { 
    www_link : Hyper.link;        (* the link that produced this request *)
    www_url : Url.t;	          (* parsed version *)
    www_fragment : string option; (* because viewer is passed down *)
    mutable www_auth : (string * string) list;  (* basic auth *)
    mutable www_headers : string list;		  (* additional headers *)
    mutable www_logging : string -> unit;	  (* logging *)
    mutable www_error : Error.t
  }

exception Invalid_request of request * string

val make : Hyper.link -> request
  (* raises: 
      Url_Lexing
      Invalid_link
   *)

(* Table of unresolved active connexions *)
module UrlSet : Set.S with type elt = Url.t

val is_active_cnx : Url.t -> bool
val add_active_cnx : Url.t -> unit
val rem_active_cnx : Url.t -> unit


(*s: ./www/www.mli *)
(*s: type Www.request *)
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
(*e: type Www.request *)

(*s: exception Www.Invalid_request *)
exception Invalid_request of request * string
(*e: exception Www.Invalid_request *)

(*s: signature Www.make *)
val make : Hyper.link -> request
  (* raises: Url_Lexing | Invalid_link *)
(*e: signature Www.make *)

(*s: signature module Www.UrlSet *)
(* Table of unresolved active connexions *)
module UrlSet : Set.S with type elt = Url.t
(*e: signature module Www.UrlSet *)

(*s: signature Www.is_active_cnx *)
val is_active_cnx : Url.t -> bool
(*e: signature Www.is_active_cnx *)
(*s: signature Www.add_active_cnx *)
val add_active_cnx : Url.t -> unit
(*e: signature Www.add_active_cnx *)
(*s: signature Www.rem_active_cnx *)
val rem_active_cnx : Url.t -> unit
(*e: signature Www.rem_active_cnx *)

(*e: ./www/www.mli *)

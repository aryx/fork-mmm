(*s: www/www.mli *)
(*s: type [[Www.request]] *)
(*
 * Requests
 *)
type request =  { 
    www_link : Hyper.link;        (* the link that produced this request *)
    mutable www_headers : string list;		  (* additional headers *)
    (*s: [[Www.request]] security field *)
    mutable www_auth : (string * string) list;  (* basic auth *)
    (*e: [[Www.request]] security field *)

    (*s: [[Www.request]] parsed link fields *)
    www_url : Url.t;	          (* parsed version *)
    www_fragment : string option; (* because viewer is passed down *)
    (*e: [[Www.request]] parsed link fields *)

    (*s: [[Www.request]] logging method *)
    mutable www_logging : string -> unit;	  (* logging *)
    (*e: [[Www.request]] logging method *)
    (*s: [[Www.request]] error managment method *)
    mutable www_error : Error.t;
    (*e: [[Www.request]] error managment method *)
  }
(*e: type [[Www.request]] *)

(*s: exception [[Www.Invalid_request]] *)
exception Invalid_request of request * string
(*e: exception [[Www.Invalid_request]] *)

(*s: signature [[Www.make]] *)
val make : Hyper.link -> request
  (* raises: Url_Lexing | Invalid_link *)
(*e: signature [[Www.make]] *)

(* Table of unresolved active connexions *)
module UrlSet : Set.S with type elt = Url.t

(*s: signature [[Www.is_active_cnx]] *)
val is_active_cnx : Url.t -> bool
(*e: signature [[Www.is_active_cnx]] *)
(*s: signature [[Www.add_active_cnx]] *)
val add_active_cnx : Url.t -> unit
(*e: signature [[Www.add_active_cnx]] *)
(*s: signature [[Www.rem_active_cnx]] *)
val rem_active_cnx : Url.t -> unit
(*e: signature [[Www.rem_active_cnx]] *)

(*s: type [[Www.aborter]] *)
type aborter = unit -> unit
(*e: type [[Www.aborter]] *)

(*e: www/www.mli *)

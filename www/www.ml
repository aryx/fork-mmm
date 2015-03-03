(*s: ./www/www.ml *)
open Uri
open Url

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
    mutable www_error : Error.t;
  }
(*e: type Www.request *)

(*s: exception Www.Invalid_request *)
exception Invalid_request of request * string
(*e: exception Www.Invalid_request *)

(*s: constant Www.sp *)
let sp = Str.regexp "[ \t\n]"
(*e: constant Www.sp *)

(*s: function Www.make *)
let make hlink =
  let absuri = Hyper.resolve hlink in 
  let url = Lexurl.make absuri.uri_url in
  try (* search for space in network URI *)
    if List.mem url.protocol [FILE; MAILTO] 
    then raise Not_found
    else
      (* will raise Not_found if no space found *)
      let n = Str.search_forward sp absuri.uri_url 0 in
      raise (Hyper.Invalid_link (Hyper.UrlLexing ("suspicious white space", n)))
  with Not_found -> 
    { www_link = hlink;

      www_url = url; (* should not fail ? *)
      www_fragment = absuri.uri_frag;

      www_auth = [];
      www_headers = [];

      www_logging = (fun _ -> ());
      www_error = !Error.default
    }
(*e: function Www.make *)
 


(*s: module Www.UrlSet *)
(* Table of unresolved active connexions *)
(* We need to keep a trace of pending connections, since there is a race
   condition when the user clicks twice rapidly on an anchor. If the second
   click occurs before the document is added to the cache, (e.g. because we
   are waiting for the headers), then the document will be retrieved twice.
   And naturally, for documents that don't enter the cache we always will
   duplicate connexions.
   Retrieve.f is a safe place to add the request to the list of pending
   connexions, because it is synchronous.
   Removing an active connexion must take place when we close the 
   dh.document_fd.
*)
module UrlSet =
  Set.Make(struct type t = Url.t let compare = compare end)
(*e: module Www.UrlSet *)

(*s: constant Www.active_connexions *)
let active_connexions = ref UrlSet.empty
(*e: constant Www.active_connexions *)
(*s: functions Www.xxx_active_cnx *)
let is_active_cnx url = 
  UrlSet.mem url !active_connexions
let add_active_cnx url = 
  active_connexions := UrlSet.add url !active_connexions
let rem_active_cnx url =
  active_connexions := UrlSet.remove url !active_connexions
(*e: functions Www.xxx_active_cnx *)

(*e: ./www/www.ml *)

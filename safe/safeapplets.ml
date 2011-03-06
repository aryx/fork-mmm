(***********************************************************************)
(*                                                                     *)
(*                           Calves                                    *)
(*                                                                     *)
(*          Francois Rouaix, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

module Capabilities = Capabilities

open Capabilities
open Document
open Hyper
open Url
open Www

(* Various copy functions to avoid nasty holes *)
(* copy a string option *)
let copy_field = function
    None -> None
  | Some s -> Some (String.copy s)

(* copy an url, possibly masking the username/password information *)
let copy_url url maskid = {
  protocol = url.protocol;
  user = (if maskid then None else copy_field url.user);
  password = (if maskid then None else copy_field url.password);
  host = copy_field url.host;
  port = url.port;
  path = copy_field url.path;
  search = copy_field url.search;
}

let copy_link link = {
  h_uri = String.copy link.h_uri;
  h_context = copy_field link.h_context;
  h_method = link.h_method;
  h_params = List.map (fun (x,y) -> String.copy x, String.copy y) link.h_params
} 

module Retrieval(C: sig val capabilities: Capabilities.t end) = struct

(* Since this is computed at initialisation time, it should be correct *)
let ask = ask C.capabilities

(* THIS MUST BE A COPY *)
(* Note: we don't reveal to the applet the user/password that was required
   to load it. The reason is that an applet written by C (and residing on
   his site), pointed to by a page on A could reveal to C the username and 
   password of B on site A. 
 *)
let whoami = copy_url C.capabilities.who true

(* Protect on the continuation, in case the initial request
   gets redirected by an accomplice server 
    e.g. http://accomplice.com/goto?someurl
     returns Location: someurl
   SECURITY NOTE: we can't let an applet handle a wr, because a wr may contain
   a username/password to some other site (Auth.check physically modifies
   the wr to add authorization information).
   SECURITY NOTE: if wr belongs to the applet world, it can be mutated between
   the emission of the request and the invocation of the continuation
   document_process. Since the Url in wr.www_url is shared with the 
   dh.document_id, we have to make our own copy or wr to be sure we check
   the propoer document_url.
  *)
let rec retrieve hlink cont = 
  let hlink = copy_link hlink in (* avoid future mutations *) 
  let wr = Www.make hlink in
  let retry hlink = retrieve hlink cont
  and real_cont = {
    document_process = (fun dh ->
      let url = Url.string_of dh.document_id.document_url in
       if ask (DocumentR url) then cont.document_process dh
       else cont.document_finish true);
    document_finish = cont.document_finish
    }
  in
  match wr.www_url.protocol with
    MAILTO -> raise Denied
  | _ ->
    if Nav.dont_check_cache wr then ignore (Retrieve.f wr retry real_cont)
    else 
      let did = {document_url = wr.www_url; document_stamp = no_stamp} in
      try
	let doc = Cache.find did in
	  real_cont.document_process (Cache.make_handle wr doc)
      with 
	Not_found -> (* we don't have the document *)
	  ignore (Retrieve.f wr retry real_cont)

(* Images are public, although they also may contain private information *)
(* JPF : progression callback is inavailable in applets *)
(*          it is just for keeping consistency *)
let get_image hlink cont =
  Img.get {document_url = C.capabilities.who; document_stamp = no_stamp}
          hlink cont Progress.no_meter

end

module Mstring = Mstring
module Mlist = Mlist
module Feed = Feed 
module Error = Error
module Hyper = Hyper
module Www = Www
module Url = Url
module Lexurl = Lexurl
module Document = Document 
module Applets = Applets 
module Viewers = Viewers

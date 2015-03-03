(*s: ./retrieve/retrieve.ml *)
(* Document retrieval *)
open Printf
open Www
open Hyper
open Url
open Document
open Http
open Http_headers
open Auth

(*s: type Retrieve.retrievalStatus *)
type retrievalStatus =
 | Started of (unit -> unit)  
 | InUse
(*e: type Retrieve.retrievalStatus *)

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

(*s: constant Retrieve.http_process *)
(*
 * Provision for user (re)definition of behaviours.
 *)
let http_process = Hashtbl.create 37
(*e: constant Retrieve.http_process *)

(*s: constant Retrieve.add_http_processor *)
let add_http_processor = Hashtbl.add http_process
(*e: constant Retrieve.add_http_processor *)

(*s: function Retrieve.wrap_cache *)
(* What do we cache ? : text/html and text/plain in memory *)
let wrap_cache cache dh =
  Log.debug (sprintf "Wrapping cache for %s(%d)"
             (Url.string_of dh.document_id.document_url)
             dh.document_id.document_stamp);
  Retype.f dh;
  try
    match Lexheaders.media_type (contenttype dh.document_headers) with
    | ("text","html"),_ 
    | ("text","plain"),_ ->
        begin 
      try
        let doc, c = cache dh in
          Cache.add dh.document_id
                 {document_address = dh.document_id.document_url;
                  document_data = doc; document_info = dh.document_headers};
          Cache.wrap c dh
          with
            Cache.DontCache -> dh
        end
    | _ -> dh
  with
    Not_found -> dh
(*e: function Retrieve.wrap_cache *)

(* 
 * Dispatch according to status code
 *  retry: how to re-emit a request
 *  cont: what to do with the response 
 *)
let rec http_check cache retry cont wwwr dh =
  Log.debug "Retrieve.http_check";
  try (* the appropriate behavior *)
    let behav = Hashtbl.find http_process dh.document_status in
      match behav wwwr dh with
    Ok -> 
      (* do I cache ? *)
      let cacheable = wwwr.www_link.h_method = GET in
            cont.document_process
          (if cacheable then wrap_cache cache dh else dh)
      | Stop msg ->
          dclose true dh;
          cont.document_finish false;
          wwwr.www_error#ok msg
      | Error msg ->
          dclose true dh;
          cont.document_finish false;
          wwwr.www_error#f msg
      | Retry hlink ->
          dclose true dh;
          cont.document_finish false;
      retry hlink
      | Restart transform ->
      dclose true dh;
          f wwwr retry {document_finish = cont.document_finish;
            document_process = (fun dh ->
                cont.document_process (transform dh))};
          () (* we should probably do something of the result ! *)
  with
    Not_found ->
     (* default behavior is to call the normal continuation 
    BUT WE DON'T CACHE !
        e.g. 404 Not found, 500, ...
      *)
      cont.document_process dh

(*
 * Emitting a request:
 *   we must catch here all errors due to protocols and remove the
 *   cnx from the set of active cnx.
 *)
and f request retry cont = 
  Log.debug "Retrieve.f";
  if Www.is_active_cnx request.www_url then InUse
  else begin
   Www.add_active_cnx request.www_url;
   try 
     let req,cache = Protos.get request.www_url.protocol in
      Started (req request
        {document_finish = cont.document_finish;
        document_process = http_check cache retry cont request})

   with Not_found ->
      Www.rem_active_cnx request.www_url;
      raise (Invalid_request (request, I18n.sprintf "unknown protocol"))
    | Http.HTTP_error s ->
      Www.rem_active_cnx request.www_url;
      raise (Invalid_request (request, I18n.sprintf "HTTP Error \"%s\"" s))
    | File.File_error s ->
      Www.rem_active_cnx request.www_url;
      raise (Invalid_request (request, s))
   end


(* In all the following, we avoid popping up dialog boxes, and use
 * wwwr logging instead. Otherwise we might get too verbose for
 * in-lined images...
 *)

(*s: function Retrieve.code200 *)
(* 200 OK *)
let code200 wwwr dh = Ok
(* 201 Created (same as 200) *)
(* 202 Accepted (same as 200) *)
(*e: function Retrieve.code200 *)

(*s: function Retrieve.code204 *)
(* 204 No Content: we should modify the headers of the referer ? *)
let code204 wwwr dh =
  Stop (I18n.sprintf "Request fulfilled.\n(%s)"
                    (status_msg dh.document_headers))
(*e: function Retrieve.code204 *)

(*s: function Retrieve.forward *)
(* 302 Moved temporarily *)
let forward wwwr dh =
  try 
   let newurl = Http_headers.location dh.document_headers in
     if (* do we forward automatically ?*)
    match wwwr.www_link.h_method with
      GET -> true
    | POST _ ->
        (* Do NOT redirect automatically if method was POST *)
        wwwr.www_error#choose (I18n.sprintf 
         "Destination for your POST request has changed\n\
              from %s\nto %s\nConfirm action ?"
            (Url.string_of wwwr.www_url) newurl)
    | _ -> true 
     then begin (* consider forwarding as a link *)
       wwwr.www_logging "Forwarding";
       Retry {h_uri = newurl;
          h_context = wwwr.www_link.h_context;
          h_method = wwwr.www_link.h_method;
          h_params = wwwr.www_link.h_params
         }
       end
     else 
      (* not forwarding a moved POST. We show the document after all,
        since some people (servers ?) use this trick to show the results
        of a POST, despite what the protocol says about this *)
       Ok
  with 
    Not_found -> 
      Error (I18n.sprintf "No Location: in forwarding header")
(*e: function Retrieve.forward *)

(*s: function Retrieve.forward_permanent *)
(* 301 Moved permanently *)
let forward_permanent wwwr dh =
  try
    let newurl = Http_headers.location dh.document_headers in
      wwwr.www_error#ok (I18n.sprintf "Document moved permanently to\n%s"
                          newurl);
      forward wwwr dh
  with
    Not_found -> 
      Error (I18n.sprintf "No Location: in forwarding header")
(*e: function Retrieve.forward_permanent *)

(* 304 : Response to a conditional GET, the document is not modified
let update wwwr dh =
   Cache.patch dh.document_id dh.document_headers;
   Stop (I18n.sprintf "Document %s has not changed.\n"
                  (Url.string_of wwwr.www_url))
Because of recursive update, this has moved elsewhere.
*)

(*s: function Retrieve.code400 *)
(* 400 Bad request *)
let code400 wwwr dh = Error (I18n.sprintf "Bad Request")
(*e: function Retrieve.code400 *)

(*s: function Retrieve.ask_auth *)
(* 401 Unauthorized *)
let ask_auth wwwr dh =
  wwwr.www_logging (I18n.sprintf "Checking authentication");
  let rawchallenge = challenge dh.document_headers in
  let challenge = 
    Lexheaders.challenge (Lexing.from_string rawchallenge) in
  let host = match wwwr.www_url.host with
     Some h -> h
   | None -> ""
  and dir = match wwwr.www_url.path with
     Some "" -> "/"
   | Some h -> Filename.dirname h
   | None -> "/" 
  and port = match wwwr.www_url.port with
     Some p -> p
   | None -> 80 (* should never happen *) in

  Auth.check wwwr challenge
      {auth_proxy = false;
       auth_host = host; 
       auth_port = port;
       auth_dir = dir;
       auth_realm = challenge.challenge_realm}
(*e: function Retrieve.ask_auth *)

(*s: function Retrieve.unauthorized *)
let unauthorized wwwr dh =
  match ask_auth wwwr dh with
    None -> (* no attempt to answer challenge, display the message *)
      Ok
  | Some (cookie, isnew, space) ->
     (* restart the request with a continuation that says first
    to check if authorization was valid, and then proceed
    to the normal intended continuation *)
     Restart (fun newdh ->
        if newdh.document_status <> 401 & isnew then
          Auth.add space cookie;
        (* Put the challenge header again *)
        begin try
          newdh.document_headers <- 
             ("WWW-Authenticate: "^ 
                   (challenge dh.document_headers))
             :: newdh.document_headers
         with
          Not_found -> ()
        end;
                newdh)
(*e: function Retrieve.unauthorized *)

(*s: function Retrieve.ask_proxy_auth *)
(* 407 Unauthorized *)
(* We dump the realm altogether, because it has no meaning for proxies *)
let ask_proxy_auth wwwr dh =
  wwwr.www_logging (I18n.sprintf "Checking proxy authentication");
  let rawchallenge = proxy_challenge dh.document_headers in
  let challenge = 
    Lexheaders.challenge (Lexing.from_string rawchallenge) in
  Auth.check wwwr challenge
      {auth_proxy = true;
       auth_host = !proxy;
       auth_port = !proxy_port;
       auth_dir = "";
       auth_realm = ""}
(*e: function Retrieve.ask_proxy_auth *)

(*s: function Retrieve.proxy_unauthorized *)
let proxy_unauthorized wwwr dh =
  Log.debug "proxy_unauthorized handler";
  match ask_proxy_auth wwwr dh with
    None -> (* no attempt to answer challenge, display the message *)
      Ok
  | Some (cookie, isnew, space) ->
     (* restart the request with a continuation that says first
    to check if authorization was valid, and then proceed
    to the normal intended continuation *)
      Restart (fun newdh -> 
         Log.debug "proxy_unauthorized wrapper";
         if newdh.document_status <> 407 & isnew then
           Auth.add space cookie;
         (* Put the challenge header again *)
         begin try
           newdh.document_headers <- 
              ("Proxy-Authenticate: "^ 
                                    (proxy_challenge dh.document_headers))
              :: newdh.document_headers
          with
           Not_found -> ()
         end;
                 newdh)
(*e: function Retrieve.proxy_unauthorized *)

(*s: toplevel Retrieve._1 *)
(* 400 : proxies do return this code when they can satisfy the request,
 *       so we keep it as default (displayed)
 *)
let _ =
 List.iter (function (code, behave) -> Hashtbl.add http_process code behave)
  [200, code200;
   201, code200;
   202, code200;
   204, code204;

   301, forward_permanent;
   302, forward;
   (* 304, update; *)
   401, unauthorized;
   407, proxy_unauthorized]
(*e: toplevel Retrieve._1 *)
(*e: ./retrieve/retrieve.ml *)

(*s: retrieve/retrieve.ml *)
open Common
open I18n

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Document retrieval *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(*s: type [[Retrieve.retrievalStatus]] *)
type status =
 | Started of Www.aborter
 | InUse
(*e: type [[Retrieve.retrievalStatus]] *)

(*s: type [[Retrieve.behaviour]] *)
(* We should implement the proper behaviours for all return codes
 * defined in the HTTP/1.0 protocol draft. 
 * Return codes are HTTP specific, but since all protocols are more or
 * less mapped to http, we deal with them at the retrieval level.
 *)
type behaviour =
 | Ok 			              (* process the document *)
 | Stop of string             (* stop (no document) and display message *)
 | Retry of Hyper.link        (* restart with a new link *)
 | Error of string            (* same as stop, but it's an error *)
 | Restart of (Document.handle -> Document.handle) 
   (* restart the same request, but apply transformation on the continuation *)
(*e: type [[Retrieve.behaviour]] *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
(*s: constant [[Retrieve.http_process]] *)
(*
 * Provision for user (re)definition of behaviours.
 *)
let http_process : 
  (int, Www.request -> Document.handle -> behaviour) Hashtbl.t = 
  Hashtbl_.create ()
(*e: constant [[Retrieve.http_process]] *)

(*s: constant [[Retrieve.add_http_processor]] *)
let add_http_processor = Hashtbl.add http_process
(*e: constant [[Retrieve.add_http_processor]] *)

(*****************************************************************************)
(* Cache *)
(*****************************************************************************)
(*s: function [[Retrieve.wrap_cache]] *)
(* What do we cache ? : text/html and text/plain in memory *)
let wrap_cache cache (dh : Document.handle) : Document.handle =
  Logs.debug (fun m -> m "Wrapping cache for %s(%d)"
             (Url.string_of dh.document_id.document_url)
             dh.document_id.document_stamp);
  (* infer content-type and content-encoding using filename extension of
   * document if content-type was not specified in the headers
   *)
  Retype.f dh;
  try
    match Lexheaders.media_type (Http_headers.contenttype dh.dh_headers) with
    | ("text","html"),_ 
    | ("text","plain"),_ ->
      begin 
       try
         let doc, c = cache dh in
         Cache.add dh.document_id
                 { document_address = dh.document_id.document_url;
                   document_data = doc; 
                   document_headers = dh.dh_headers };
          Cache.wrap c dh
       with Cache.DontCache -> dh
      end
    | _ -> dh
  with Not_found -> dh
(*e: function [[Retrieve.wrap_cache]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*s: function [[Retrieve.http_check]] *)
(* 
 * Dispatch according to status code
 *  retry: how to re-emit a request
 *  cont: what to do with the response 
 *)
(* f -> Http.req (via protos) cont -> <> (via cont.document_process) -> codex *)
let rec http_check (caps: < Cap.network; .. > )
         cache (retry : Hyper.link -> unit)
         (cont : Document.continuation) (wwwr : Www.request)
         (dh : Document.handle) : unit =
  Logs.debug (fun m -> m "Retrieve.http_check");
  try (* the appropriate behavior *)
    (* alt: just have a single function matching on code *)
    let behav = Hashtbl.find http_process dh.document_status in
    match behav wwwr dh with
    | Ok -> 
        (* do I cache ? *)
        let cacheable = wwwr.www_link.h_method = GET in
        cont.document_process
          (if cacheable then wrap_cache cache dh else dh)
    | Stop msg ->
        Document.dclose true dh;
        cont.document_finish false;
        wwwr.www_error#ok msg
    | Error msg ->
        Document.dclose true dh;
        cont.document_finish false;
        wwwr.www_error#f msg
    | Retry hlink ->
        Document.dclose true dh;
        cont.document_finish false;
        retry hlink
    | Restart transform ->
        Document.dclose true dh;
        f caps wwwr retry 
         { cont with 
           document_process = (fun dh -> cont.document_process (transform dh))}
        |> ignore; (* we should probably do something of the result ! *)
  with Not_found ->
     (* default behavior is to call the normal continuation 
      * BUT WE DON'T CACHE !
      * e.g. 404 Not found, 500, ...
      *)
      cont.document_process dh
(*e: function [[Retrieve.http_check]] *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*s: function [[Retrieve.f]] *)
(*
 * Emitting a request:
 *   we must catch here all errors due to protocols and remove the
 *   cnx from the set of active cnx.
 *)
(* Nav.request -> <> -> Http.req (via Protos.get) -> Http.tcp_connect *)
and f (caps : < Cap.network; ..>) 
      (request : Www.request) (retry : Hyper.link -> unit) 
      (cont : Document.continuation) : status = 
  Logs.debug (fun m -> m "Retrieve.f on %s" (Url.string_of request.www_url));
  if Www.is_active_cnx request.www_url
  then InUse
  else begin
    Www.add_active_cnx request.www_url;
    try 
      let (reqf, cachef) = Protos.get request.www_url.protocol in
      Started (reqf (caps :> < Cap.network >) request
                 { cont with
                   document_process = http_check caps cachef retry cont request})

   with 
   | Not_found ->
       Www.rem_active_cnx request.www_url;
       raise (Www.Invalid_request (request, s_ "unknown protocol"))
   | Http.HTTP_error s ->
       Www.rem_active_cnx request.www_url;
       raise (Www.Invalid_request (request, s_ "HTTP Error \"%s\"" s))
   | File.File_error s ->
       Www.rem_active_cnx request.www_url;
       raise (Www.Invalid_request (request, s))
   end
(*e: function [[Retrieve.f]] *)


(*****************************************************************************)
(* Response code -> behavior *)
(*****************************************************************************)

(* In all the following, we avoid popping up dialog boxes, and use
 * wwwr logging instead. Otherwise we might get too verbose for
 * in-lined images...
 *)

(*s: function [[Retrieve.code200]] *)
(* 200 OK *)
let code200 _wwwr _dh = Ok
(* 201 Created (same as 200) *)
(* 202 Accepted (same as 200) *)
(*e: function [[Retrieve.code200]] *)

(*s: function [[Retrieve.code204]] *)
(* 204 No Content: we should modify the headers of the referer ? *)
let code204 _wwwr (dh : Document.handle) =
  Stop (s_ "Request fulfilled.\n(%s)" (Http_headers.status_msg dh.dh_headers))
(*e: function [[Retrieve.code204]] *)

(*s: function [[Retrieve.forward]] *)
(* 302 Moved temporarily *)
let forward (wr : Www.request) (dh : Document.handle) =
  try 
    let newurl = Http_headers.location dh.dh_headers in
    if (* do we forward automatically ?*)
      match wr.www_link.h_method with
      | GET -> true
      | POST _ ->
         (* Do NOT redirect automatically if method was POST *)
         wr.www_error#choose (s_ "Destination for your POST request has changed\n  from %s\nto %s\nConfirm action ?"
            (Url.string_of wr.www_url) newurl)
      | _ -> true 
    then begin (* consider forwarding as a link *)
       wr.www_logging "Forwarding";
       Retry { wr.www_link with h_uri = newurl; }
    end else 
      (* not forwarding a moved POST. We show the document after all,
        since some people (servers ?) use this trick to show the results
        of a POST, despite what the protocol says about this *)
       Ok
  with Not_found -> 
    Error (s_ "No Location: in forwarding header")
(*e: function [[Retrieve.forward]] *)

(*s: function [[Retrieve.forward_permanent]] *)
(* 301 Moved permanently *)
let forward_permanent (wr : Www.request) (dh : Document.handle) =
  try
    let newurl = Http_headers.location dh.dh_headers in
    wr.www_error#ok (s_ "Document moved permanently to\n%s" newurl);
    forward wr dh
  with Not_found -> 
    Error (s_ "No Location: in forwarding header")
(*e: function [[Retrieve.forward_permanent]] *)

(* 304 : Response to a conditional GET, the document is not modified
let update wr dh =
   Cache.patch dh.document_id dh.dh_headers;
   Stop (s_ "Document %s has not changed.\n" (Url.string_of wr.www_url))
Because of recursive update, this has moved elsewhere.
*)

(*s: function [[Retrieve.code400]] *)
(* 400 Bad request *)
let _code400 _wr _dh = Error (s_ "Bad Request")
(*e: function [[Retrieve.code400]] *)

(*s: function [[Retrieve.ask_auth]] *)
(* 401 Unauthorized *)
let ask_auth (wr : Www.request) (dh : Document.handle) =
  wr.www_logging (s_ "Checking authentication");
  let rawchallenge = Http_headers.challenge dh.dh_headers in
  let challenge = 
    Lexheaders.challenge (Lexing.from_string rawchallenge) in
  let host = match wr.www_url.host with
     Some h -> h
   | None -> ""
  and dir = match wr.www_url.path with
     Some "" -> "/"
   | Some h -> Filename.dirname h
   | None -> "/" 
  and port = match wr.www_url.port with
     Some p -> p
   | None -> 80 (* should never happen *) in

  Auth.check wr challenge
      {auth_proxy = false;
       auth_host = host; 
       auth_port = port;
       auth_dir = dir;
       auth_realm = challenge.challenge_realm}
(*e: function [[Retrieve.ask_auth]] *)

(*s: function [[Retrieve.unauthorized]] *)
let unauthorized wr dh =
  match ask_auth wr dh with
    None -> (* no attempt to answer challenge, display the message *)
      Ok
  | Some (cookie, isnew, space) ->
     (* restart the request with a continuation that says first
    to check if authorization was valid, and then proceed
    to the normal intended continuation *)
     Restart (fun newdh ->
        if newdh.document_status <> 401 && isnew then
          Auth.add space cookie;
        (* Put the challenge header again *)
        begin try
          newdh.dh_headers <- ("WWW-Authenticate: "^ (Http_headers.challenge dh.dh_headers))
                              :: newdh.dh_headers
         with
          Not_found -> ()
        end;
                newdh)
(*e: function [[Retrieve.unauthorized]] *)

(*s: function [[Retrieve.ask_proxy_auth]] *)
(* 407 Unauthorized *)
(* We dump the realm altogether, because it has no meaning for proxies *)
let ask_proxy_auth (wr : Www.request) (dh : Document.handle) =
  wr.www_logging (s_ "Checking proxy authentication");
  let rawchallenge = Http_headers.proxy_challenge dh.dh_headers in
  let challenge = 
    Lexheaders.challenge (Lexing.from_string rawchallenge) in
  Auth.check wr challenge
      {auth_proxy = true;
       auth_host = !Http.proxy;
       auth_port = !Http.proxy_port;
       auth_dir = "";
       auth_realm = ""}
(*e: function [[Retrieve.ask_proxy_auth]] *)

(*s: function [[Retrieve.proxy_unauthorized]] *)
let proxy_unauthorized wr dh =
  Log.debug "proxy_unauthorized handler";
  match ask_proxy_auth wr dh with
    None -> (* no attempt to answer challenge, display the message *)
      Ok
  | Some (cookie, isnew, space) ->
     (* restart the request with a continuation that says first
    to check if authorization was valid, and then proceed
    to the normal intended continuation *)
      Restart (fun newdh -> 
         Log.debug "proxy_unauthorized wrapper";
         if newdh.document_status <> 407 && isnew then
           Auth.add space cookie;
         (* Put the challenge header again *)
         begin try
           newdh.dh_headers <- 
              ("Proxy-Authenticate: "^ (Http_headers.proxy_challenge dh.dh_headers))
              :: newdh.dh_headers
          with
           Not_found -> ()
         end;
                 newdh)
(*e: function [[Retrieve.proxy_unauthorized]] *)

(*s: toplevel [[Retrieve._1]] *)
(* 400 : proxies do return this code when they can satisfy the request,
 *       so we keep it as default (displayed)
 *)
let _ =
  [200, code200;
   201, code200;
   202, code200;
   204, code204;
   (*s: Retrieve code behaviour other elements *)
   301, forward_permanent;
   302, forward;
   (* 304, update; *)
   (*x: Retrieve code behaviour other elements *)
   401, unauthorized;
   407, proxy_unauthorized;
   (*e: Retrieve code behaviour other elements *)
  ] |> List.iter (function (code, behave) -> 
     Hashtbl.add http_process code behave
  )
(*e: toplevel [[Retrieve._1]] *)
(*e: retrieve/retrieve.ml *)

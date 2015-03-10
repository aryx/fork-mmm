(*s: ./http/http.ml *)
(* Retrieve an HTTP document *)
open I18n
open Unix
open Www
open Hyper
open Auth
open Document
open Feed
open Messages
open Http_headers
open Url

(*s: constant Http.always_proxy *)
let always_proxy = ref false
(*e: constant Http.always_proxy *)
(*s: constant Http.timeout *)
let timeout = ref 60		(* in seconds *)
(*e: constant Http.timeout *)

(*s: global Http.proxy *)
(* Default proxy definitions *)
let proxy = ref "no-proxy-defined"
(*e: global Http.proxy *)
(*s: global Http.proxy_port *)
let proxy_port = ref 80
(*e: global Http.proxy_port *)

(*s: constant Http.verbose *)
let verbose = ref false
(*e: constant Http.verbose *)

(*s: exception Http.HTTP_error *)
exception HTTP_error of string
(*e: exception Http.HTTP_error *)

(*s: type Http.status *)
(* Support for aborting requests while in connect/write/headers mode.
   When we start applying the document continuation, it is not our job
   anymore to abort the connection.
 *)
type status = 
  | Writing 
  (*s: [[Http.status]] cases *)
  | Reading of handle 
  (*x: [[Http.status]] cases *)
  | Discharged
  (*e: [[Http.status]] cases *)
(*e: type Http.status *)

(*s: class Http.cnx *)
class cnx (sock, finish) =
 object (self)
  val mutable status = Writing
  val mutable fd = sock

  (* val finish = finish *)
  val mutable fdclosed = false		(* protect against double close *)
  val mutable aborted = false

  method fd = 
    fd
  method aborted = 
    aborted
  method set_fd newfd = 
    fd <- newfd
  method set_status s = 
    status <- s

  method close =
    if not fdclosed then begin
      close fd;
      fdclosed <- true
    end

  (* can be called from the aborter by the user or some exn handler *)  
  method abort =
     if not aborted then begin
       aborted <- true;
       match status with
       | Writing -> 
           Fileevent_.remove_fileoutput fd; 
           self#close; 
           finish true
       (*s: [[Http.cnx.abort()]] cases *)
       | Discharged -> ()
       (*x: [[Http.cnx.abort()]] cases *)
       | Reading dh -> 
           Document.dclose true dh; 
           finish true
       (*e: [[Http.cnx.abort()]] cases *)
    end
end
(*e: class Http.cnx *)


(*s: function Http.tcp_connect *)
(* Open a TCP connection, asynchronously (except for DNS).
   We pass the continuation *)
let tcp_connect server_name port  log  cont error =

  (*  Find the inet address *)
  let server_addr =
    try Unix.inet_addr_of_string server_name
    with Failure _ ->
      (*s: [[Http.tcp_connect()]] if inet_add_of_string fails *)
      try
        log (s_ "Looking for %s ..." server_name);
        let adr = (Low.busy Munix.gethostbyname server_name).h_addr_list.(0) in
        log (s_ "%s found" server_name);
        adr
      with Not_found -> 
       raise (HTTP_error (s_ "Unknown host: %s" server_name)) 
      (*e: [[Http.tcp_connect()]] if inet_add_of_string fails *)
  in

  (* Attempt to connect *)
  let sock = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.clear_nonblock sock;
  Unix.set_nonblock sock; (* set to non-blocking *)
  let cnx = new cnx (sock, error "User abort") in
  log (s_ "Contacting host...");
  try
    begin try
      Unix.connect sock (ADDR_INET(server_addr, port));
      (* just in case. Normally an error should be raised *)
      Unix.clear_nonblock sock; (* set to non-blocking *)
      log (s_ "connection established");
      Log.debug "Connect returned without error !";

      (* because we need to return cnx *)
      Timer_.set 10 (fun () -> 
        (* ! calling the continuation, e.g. start_request *)
        cont cnx
      );
      cnx
    with Unix_error((EINPROGRESS | EWOULDBLOCK | EAGAIN), "connect", _) -> 
      (*s: [[Http.tcp_connect()]] if unix error when connect *)
       (* that is ok, we are starting something *)
       let stuck = ref true in
       Fileevent_.add_fileoutput sock
         (* we are called when the cnx is established *)
         (fun () -> 
           stuck := false;
           Fileevent_.remove_fileoutput sock;
           Unix.clear_nonblock sock; (* return to blocking mode *)
           begin try (* But has there been a cnx actually *)
             let _ = getpeername sock in
             log (s_ "connection established");
             cont cnx
           with Unix_error(ENOTCONN, "getpeername", _) ->
             cnx#close;
             error (s_ "Connection refused to %s" server_name) false
            end
         );
        (*s: [[Http.tcp_connect()]] setup timeout *)
        (* but also start the timer if nothing happens now
        * the kernel has a timeout, but it might be too long (linux) 
        *)
        Timer_.set (1000 * !timeout)
          (fun () -> 
             if not cnx#aborted && !stuck 
             then begin
               Fileevent_.remove_fileoutput sock;
               cnx#close;
               error (s_ "Timeout during connect to %s" server_name) false
              end
          );
        (*e: [[Http.tcp_connect()]] setup timeout *)
        cnx
      (*e: [[Http.tcp_connect()]] if unix error when connect *)
    end
  with Unix_error(e,fn,_) ->  (* other errors in connect *)
    cnx#close;
    raise (HTTP_error (s_ "Cannot establish connection\n%s:%s"
                             (error_message e) fn))
(*e: function Http.tcp_connect *)

(*s: constant Http.send_referer *)
(*
 * HTTP/1.0
 * Headers should be configurable
 *)

let send_referer = ref false
(*e: constant Http.send_referer *)
(*s: constant Http.user_agent *)
let user_agent = 
  ref Version.http
(*e: constant Http.user_agent *)

(*s: function Http.std_request_headers *)
let std_request_headers() =
  Printf.sprintf "User-Agent: %s\r\n" !user_agent
(*e: function Http.std_request_headers *)

(*s: function Http.full_request *)
let full_request w proxy_mode wwwr = 
  let url = 
    (*s: [[Http.full_request()]] url value if proxy mode *)
    if proxy_mode 
    then Url.string_of wwwr.www_url
    (*e: [[Http.full_request()]] url value if proxy mode *)
    else distant_path wwwr.www_url 
  in
  (*s: [[Http.full_request()]] helper functions *)
  let write_other_headers () =
    wwwr.www_headers |> List.iter (fun s -> w s; w "\r\n");
    (* If no Accept given in request, write default one *)
    begin
      try
        get_header "accept" wwwr.www_headers |> ignore
      with Not_found -> w "Accept: */*\r\n"
    end
  in
  (*x: [[Http.full_request()]] helper functions *)
  (* Host: header for virtual domains *)
  let write_host () =
    match wwwr.www_url.host with
    | None -> (* never happens *) ()
    | Some h -> 
        (match wwwr.www_url.port with
        | None -> w ("Host: "^h^"\r\n")
        | Some p ->  w ("Host: "^h^":"^string_of_int p^"\r\n")
        )
  in
  (*x: [[Http.full_request()]] helper functions *)
  let write_referer = 
    match wwwr.www_link.h_context with
    | None -> 
      (fun () -> ())
    | Some r -> 
       (fun () -> 
         if !send_referer 
         then w ("Referer: " ^ r ^ "\r\n"))
  in
  (*x: [[Http.full_request()]] helper functions *)
  (* If the request has an Authorization, write it *)
  let write_realm_auth () =
    try
      let cookie = List.assoc "realm" wwwr.www_auth in
      w ("Authorization: Basic "^cookie^"\r\n")
    with Not_found -> ()
  in
  (*x: [[Http.full_request()]] helper functions *)
  (* For proxy, we don't wait until we get an authorization error *)
  let write_proxy_auth () =
    let authspace = {
      auth_proxy = true;
      auth_host = !proxy;
      auth_port = !proxy_port;
      auth_dir = "";
      auth_realm = ""} 
    in
    try (* do we know the cookie *)
      let cookie = Auth.get authspace in
      w ("Proxy-Authorization: Basic "^cookie^"\r\n")
    with Not_found -> (* is that in the request ? *)
       try
         let cookie = List.assoc "proxy" wwwr.www_auth in
         w ("Proxy-Authorization: Basic "^cookie^"\r\n")
       with Not_found -> ()
  in
  (*e: [[Http.full_request()]] helper functions *)
  match wwwr.www_link.h_method with
  (*s: [[Http.full_request()]] method cases *)
  | GET ->
      w ("GET " ^ url ^ " HTTP/1.0\r\n");
      (* No General-Header *)
      w (std_request_headers());
      write_referer ();
      (*s: [[Http.full_request()]] write auth stuff *)
      write_realm_auth ();
      if proxy_mode 
      then write_proxy_auth();
      (*e: [[Http.full_request()]] write auth stuff *)
      write_other_headers();
      write_host();
      w "\r\n"
  (*x: [[Http.full_request()]] method cases *)
  | POST data ->
      w ("POST "^url^" HTTP/1.0\r\n");
      (* No General-Header *)
      w (std_request_headers());
      write_referer ();
      (*s: [[Http.full_request()]] write auth stuff *)
      write_realm_auth ();
      if proxy_mode 
      then write_proxy_auth();
      (*e: [[Http.full_request()]] write auth stuff *)
      write_other_headers();
      write_host();
      (* 8.2.1 *)
      w ("Content-Type: application/x-www-form-urlencoded\r\n");
      (* 7.2 note *)
      w ("Content-Length: " ^ string_of_int (String.length data)^ "\r\n");
      w "\r\n";
      w data
  (*x: [[Http.full_request()]] method cases *)
  | HEAD ->
      w ("HEAD "^url^" HTTP/1.0\r\n");
      (* No General-Header *)
      w (std_request_headers());
      write_referer ();
      (*s: [[Http.full_request()]] write auth stuff *)
      write_realm_auth ();
      if proxy_mode 
      then write_proxy_auth();
      (*e: [[Http.full_request()]] write auth stuff *)
      write_other_headers();
      write_host();
      w "\r\n"
  (*e: [[Http.full_request()]] method cases *)
(*e: function Http.full_request *)


(*s: function Http.failed_request *)
(* shared error *)
let failed_request wr finish =
 fun s aborted ->
  finish aborted;
  Www.rem_active_cnx wr.www_url;
  wr.www_logging (s_ "Failed");
  wr.www_error#f (s_ "Request for %s failed\n%s" (Url.string_of wr.www_url) s)
(*e: function Http.failed_request *)


(*
 *  Process an HTTP request asynchronously
 *)

(*s: exception Http.End_of_headers *)
exception End_of_headers
(*e: exception Http.End_of_headers *)

(*s: function Http.read_headers *)
let read_headers fd previous =
  let l = Munix.read_line fd in
   if String.length l = 0 
   then raise End_of_headers (* end of headers *)
   else 
     if l.[0] = ' ' || l.[0] = '\t' 
     then  (* continuation *)
       match previous with
       | [] -> raise (Invalid_HTTP_header ("invalid continuation " ^ l))
       | s :: rest -> (s^l) :: rest
      else l :: previous
(*e: function Http.read_headers *)


(*s: function Http.process_response *)
(* Read headers and run continuation *)
let rec process_response wwwr cont =
 fun cnx ->
  let url = Url.string_of wwwr.www_url in
  wwwr.www_logging (s_ "Reading headers...");

  let dh = 
    { document_id = document_id wwwr;
      document_referer = wwwr.www_link.h_context;
      document_fragment = wwwr.www_fragment;

      document_status = 0;
      dh_headers = [];
      document_feed = Feed.of_fd cnx#fd;

      document_logger = Document.tty_logger;
    }
  in
  cnx#set_status (Reading dh);

  let stuck = ref true in
  (* set up a timer to abort if server is too far/slow *)
  (*s: [[Http.process_response()]] setup a timer *)
  let rec timout () =
     Timer_.set (1000 * !timeout) 
      (fun () -> 
         if not cnx#aborted && !stuck 
         then
           match 
            wwwr.www_error#ari (s_ "Timeout while waiting for headers of %s" url) 
           with
           | 0 -> (* abort *) if !stuck then cnx#abort
           | 1 -> (* retry *) timout ()
           | 2 -> (* ignore *) ()
           | _ -> ()
    ) 
  in

  timout();
  (*e: [[Http.process_response()]] setup a timer *)

  (* reading the headers *)
  dh.document_feed.feed_schedule
    (fun () ->
       stuck := false;
       (*s: [[Http.process_response()]] reading headers *)
       try
         if dh.dh_headers = [] then begin
           (* it should be the HTTP Status-Line *)
           let l = Munix.read_line cnx#fd in
           dh.document_status <- (Http_headers.parse_status l).status_code;
           dh.dh_headers <- [l] (* keep it there *)
         end else 
            dh.dh_headers <- read_headers cnx#fd dh.dh_headers
       with
       (* each branch must unschedule *)
       (*s: [[Http.process_response()]] feed schedule callback failure cases *)
       | End_of_headers ->
           dh.document_feed.feed_unschedule();
           cnx#set_status Discharged;

           (* ! call the continuation, e.g. MMM stuff? *)
           cont.document_process dh
       (*x: [[Http.process_response()]] feed schedule callback failure cases *)
       | Not_found -> (* that's what parse_status raises. HTTP/0.9 dammit *)
           dclose false dh; (* keep it an active cnx since we are retrying *)
           let newcnx = request09 wwwr cont in
           (* the guy up there has the old one !*)
           cnx#set_fd newcnx#fd
       (*x: [[Http.process_response()]] feed schedule callback failure cases *)
       | Unix_error(e,_,_) ->
           cnx#abort;
           wwwr.www_error#f (s_ "Error while reading headers of %s\n%s" url 
                                  (error_message e))
       (*x: [[Http.process_response()]] feed schedule callback failure cases *)
       | Invalid_HTTP_header s ->
           cnx#abort;
           wwwr.www_error#f (s_ "Error while reading headers of %s\n%s" url s)
       (*x: [[Http.process_response()]] feed schedule callback failure cases *)
       | End_of_file ->
           cnx#abort;
           wwwr.www_error#f (s_ "Error while reading headers of %s\n%s" url "eof"))
       (*e: [[Http.process_response()]] feed schedule callback failure cases *)
       (*e: [[Http.process_response()]] reading headers *)
(*e: function Http.process_response *)

(* The same for HTTP 0.9, so we directly call the continuation *)
and process_response09  wwwr cont cnx =
   let dh =
       { document_id = document_id wwwr;
         document_referer = wwwr.www_link.h_context;
         document_status = 200;
         dh_headers = ["Content-Type: text/html"];
         document_feed = Feed.of_fd cnx#fd;
         document_fragment = wwwr.www_fragment;
         document_logger = tty_logger} 
   in
   cnx#set_status Discharged;
   cont.document_process dh

(*s: function Http.async_request *)
(* Writing the request to the server
 *   TODO:  We might get some error here in write
 *   NOTE: tk doesn't allow two handles on the same fd, thus use CPS
 *         so that reading response is our continuation
 *)
and async_request proxy_mode wwwr cont cnx =
  let b = Ebuffer.create 1024 in
  full_request (fun x -> Ebuffer.output_string b x) proxy_mode wwwr;
  let req = Ebuffer.get b in
  let len = Ebuffer.used b in
  let curpos = ref 0 in
  wwwr.www_logging (s_ "Writing request...");
  Fileevent_.add_fileoutput cnx#fd (fun _ ->
    let n = Unix.write cnx#fd req !curpos (len - !curpos) in (* blocking ? *)
    curpos := !curpos  + n;
    if !curpos = len then begin
      Fileevent_.remove_fileoutput cnx#fd;
      (*s: [[Http.async_request()]] log request string req if verbose *)
      if !verbose 
      then Log.f req;
      (*e: [[Http.async_request()]] log request string req if verbose *)
      (* ! calling the continuation, e.g. process_response *)
      cont cnx
    end)
(*e: function Http.async_request *)


(* wrappers for request/response transaction *)
(*s: function Http.start_request *)
and start_request proxy_mode wwwr cont =
 fun cnx ->
  async_request proxy_mode wwwr 
     (fun cnx -> process_response wwwr cont cnx) cnx
(*e: function Http.start_request *)

and start_request09 proxy_mode wwwr cont cnx =
  async_request proxy_mode wwwr (process_response09 wwwr cont) cnx


(*s: function Http.proxy_request *)
(* Process an HTTP request using the proxy. We pass on the continuation *)
and proxy_request wr cont =
  tcp_connect !proxy !proxy_port wr.www_logging
       (start_request true wr cont)
       (failed_request wr cont.document_finish)
(*e: function Http.proxy_request *)

and proxy_request09 wr cont =
  tcp_connect !proxy !proxy_port wr.www_logging
          (start_request09 true wr cont)
          (failed_request wr cont.document_finish)

(*s: function Http.request *)
(* Issueing request, with the "retry" logic (unless is "always proxy" mode, 
   we attempt first to connect directly to the host, and if it fails,
   we retry through the proxy
 *)
and request wr cont =
  (*s: [[Http.request()]] if always proxy *)
  if !always_proxy 
  then proxy_request wr cont
  (*e: [[Http.request()]] if always proxy *)
  else 
    let urlp = wr.www_url in
    if urlp.protocol = HTTP 
    then
      let host = 
        match urlp.host with
        | Some h -> h 
        | _ -> raise (HTTP_error (s_ "Missing host in url"))
      in
      let port =  
        match urlp.port with
        | Some p -> p
        | None -> 80  (* default http port *)
      in 
      try 
        tcp_connect host port wr.www_logging
            (fun cnx -> start_request false wr cont  cnx)
            (fun s aborted -> failed_request wr cont.document_finish s aborted)

      with HTTP_error _ -> (* direct failed, go through proxy *)
        (*s: [[Http.request()]] if http error on tcp_connect, try proxy *)
        proxy_request wr cont
        (*e: [[Http.request()]] if http error on tcp_connect, try proxy *)
    else 
      raise (HTTP_error (s_ 
             "INTERNAL ERROR\nHttp.request (not a distant http url): %s" 
               (Url.string_of wr.www_url)))
(*e: function Http.request *)

and request09 wr cont =
  if !always_proxy 
  then proxy_request09 wr cont
  else 
    let urlp = wr.www_url in
    if urlp.protocol = HTTP then
      let host = 
        match urlp.host with
        | Some h -> h 
        | _ -> raise (HTTP_error (s_ "Missing host in url"))
      in
      let port = 
        match urlp.port with
        | Some p -> p
        | None -> 80  (* default http port *)
      in
      try 
       tcp_connect host port wr.www_logging
        (start_request09 false wr cont)
        (failed_request wr cont.document_finish)
      with HTTP_error _ ->
        tcp_connect !proxy !proxy_port wr.www_logging
               (start_request09 true wr cont)
              (failed_request wr cont.document_finish)
    else 
      raise (HTTP_error (s_ "INTERNAL ERROR\nHttp.request09 (not a distant http url): %s" (Url.string_of wr.www_url)))

(* Wrappers returning the abort callback *)
(*s: function Http.req *)
let req wr cont =
  let cnx = request wr cont in
  (fun () -> cnx#abort)
(*e: function Http.req *)
(*s: function Http.prox_req *)
and proxy_req wr cont = 
  let cnx = proxy_request wr cont in
  (fun () -> cnx#abort)
(*e: function Http.prox_req *)
(*e: ./http/http.ml *)

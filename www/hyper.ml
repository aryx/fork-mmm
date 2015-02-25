(*s: ./www/hyper.ml *)
open Printf
open Mstring
open Uri
open Url

(* An hypertext(media) link on the Web *)

(*s: type Hyper.link_method *)
(* This is currently for HTTP and derived, but ... *)
(* Contains only the one we support *)
type link_method =
   GET 
 | HEAD
 | POST of string
(*e: type Hyper.link_method *)

(*s: function Hyper.parse_method *)
let parse_method = function
   "GET" -> GET
 | "HEAD" -> HEAD
 | "POST" -> POST ""
 | _ -> raise Not_found (* other cases should be caught by caller ! *)
(*e: function Hyper.parse_method *)


(*s: type Hyper.link *)
(* An hypertext(media) link on the Web *)
type link = {
  h_uri : string;
  h_context: string option;

  h_method : link_method;		(* default is GET *)
  h_params : (string * string) list
  }
(*e: type Hyper.link *)

(*s: type Hyper.link_error *)
type link_error =
    LinkResolve of string
  | UrlLexing of string * int
(*e: type Hyper.link_error *)

(*s: exception Hyper.Invalid_link *)
exception Invalid_link of link_error
(*e: exception Hyper.Invalid_link *)

(*s: function Hyper.urlconcat *)
(* parsed Absolute URL + URL -> Absolute URL *)
(* NO FRAGMENT HANDLING *)

let urlconcat contextp newuri =
  let l = String.length newuri in 
    if l = 0 then string_of contextp 
    else if l > 2 & newuri.[0] = '/' & newuri.[1] = '/' then
      (* this is probably a gopher relative uri *)
      sprintf "%s:%s" (string_of_protocol contextp.protocol) newuri
    else if newuri.[0] = '/' then (* start from root *)
      string_of {
        protocol = contextp.protocol;
     user = contextp.user;
     password = contextp.password;
        host = contextp.host;
        port = contextp.port;
     path = Some (Urlenc.unquote 
                (String.sub newuri 1 (String.length newuri - 1)));
     search = None }
    else if newuri.[0] = '?' then (* change only search part *)
      string_of {
        protocol = contextp.protocol;
     user = contextp.user;
     password = contextp.password;
        host = contextp.host;
        port = contextp.port;
     path = contextp.path;
     search = Some (String.sub newuri 1 (String.length newuri - 1))}
    else 
      let pathpart,searchpart =
    try
      let n = String.index newuri '?' in
      String.sub newuri 0 n,
      Some (String.sub newuri (n+1) (l - n - 1))
    with
      Not_found -> newuri, None
      in
      match contextp.path with
      None | Some "" -> 
        string_of {
        protocol = contextp.protocol;
        user = contextp.user;
        password = contextp.password;
        host = contextp.host;
        port = contextp.port;
        path = Some (Urlenc.unquote (Lexurl.remove_dots pathpart));
        search = searchpart}
    | Some old ->
        (* only the "dirname" part of the context path is important *)
        (* e.g  .../d/e/f becomes /d/e/ *)
       let path = sprintf "%s/%s" (Filename.dirname old) pathpart in
        (* we then have to remove dots *)
    let reduced = Lexurl.remove_dots path in
        string_of {
        protocol = contextp.protocol;
        user = contextp.user;
        password = contextp.password;
        host = contextp.host;
        port = contextp.port;
        path = Some (Urlenc.unquote reduced);
        search = searchpart}
(*e: function Hyper.urlconcat *)
          
(*s: function Hyper.resolve *)
(* Produces an URL *)
let resolve link =
  (* First remove the possible fragment of the uri *)
  let newuri, frag =
    try
      let pos = String.index link.h_uri '#' in
       String.sub link.h_uri 0 pos, 
        Some (String.sub link.h_uri (succ pos) 
                    (String.length link.h_uri - pos - 1))
    with
        Not_found -> link.h_uri, None 
  in
  if Uri.is_absolute newuri then
    try
     {uri_url = Lexurl.normalize newuri;
      uri_frag = frag}
    with
      Url_Lexing _ ->
    raise (Invalid_link
              (LinkResolve (I18n.sprintf "not a legal absolute uri")))

  else begin (* It is a relative uri *)
    let context =
      match link.h_context with 
     None -> 
      raise (Invalid_link (LinkResolve (I18n.sprintf 
                  "no context and not an absolute url")))
       | Some c -> c in

    let contextp = 
       try Lexurl.maken context
       with
    Url_Lexing (err,pos) ->
     raise (Invalid_link (UrlLexing (err,pos)))
       in
    {uri_url = urlconcat contextp newuri;
     uri_frag = frag}
     end
(*e: function Hyper.resolve *)

(*s: function Hyper.string_of *)
let string_of link =
  let uri = resolve link in
   match uri.uri_frag with 
      None -> uri.uri_url
    | Some f -> Printf.sprintf "%s#%s" uri.uri_url f
(*e: function Hyper.string_of *)
(*e: ./www/hyper.ml *)

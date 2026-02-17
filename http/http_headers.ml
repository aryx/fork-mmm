(*s: http/http_headers.ml *)
open Printf
open Str
open Mstring
open Messages

(*s: exception [[Http_headers.Invalid_HTTP_header]] *)
exception Invalid_HTTP_header of string
(*e: exception [[Http_headers.Invalid_HTTP_header]] *)

(*s: function [[Http_headers.parse_status]] *)
(* Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF *)
let parse_status s =
 if String.length s > 5 && String.sub s 0 5 = "HTTP/" 
 then
   try
    match Str.bounded_split (regexp "[ ]") s 3 with
      [v;c;m] ->
           { status_version = v;
             status_code = int_of_string c;
             status_message = m }
    (* it happened once with Server: Netscape-Commerce/1.1 *)
    (* where the Status Line was: HTTP/1.0 302 *)
    | [v;c] ->
           { status_version = v;
             status_code = int_of_string c;
             status_message = "empty" }
    | _ -> raise (Invalid_HTTP_header "Status-Line")
   with Failure "int_of_string" -> raise (Invalid_HTTP_header "Status-Line")
 else (* 0.9, dammit *)
   raise Not_found
(*e: function [[Http_headers.parse_status]] *)

(* Request-Line = Method SP Request-URI SP HTTP-Version CRLF *)
(*s: function [[Http_headers.parse_request]] *)
(* CHECK: Normally the URI should be encoded (no spaces ?) *)
let parse_request s =
 try
  match Str.bounded_split (regexp "[ ]") s 3 with
    [m;r;v] ->
         { request_version = v;
           request_method = m;
           request_uri = r }
  | ["GET"; uri] ->
         { request_version = "HTTP/0.9";
           request_method = "GET";
           request_uri = uri }
  | [m;s] -> (* uri omitted ? *)
         { request_version = s;
           request_method = m;
           request_uri = "/" }
  | _ -> raise (Invalid_HTTP_header "Request-Line")
 with
   Failure "int_of_string" -> raise (Invalid_HTTP_header "Request-Line")
(*e: function [[Http_headers.parse_request]] *)


(*s: function [[Http_headers.get_header]] *)
(* [get_header field-name headers]
 *   returns, if it exists the field value of the header field-name
 * This is a bit costly though, but we keep headers as plain strings.
 * CHECK: speed up with some regexp matching.
 * HYP: field-name in lower-case
 *)
let get_header field_name = 
  let size = String.length field_name in
  let rec search = function
   | [] -> raise Not_found
   | s::l ->
      if String.length s >= size + 2 (* : SP *) && 
         String.lowercase_ascii (String.sub s 0 size) = field_name
      then String.sub s (size + 2) (String.length s - size - 2)
      else search l 
  in
  search
(*e: function [[Http_headers.get_header]] *)

(*s: function [[Http_headers.get_multi_header]] *)
(* [get_multi_header field_name headers]
 *   get all values of the header
 *)
let get_multi_header field_name =
  let size = String.length field_name in (* :SP *)
  let rec search = function
     [] -> []
   | s::l ->
    if   String.length s >= size + 2 (* : SP *)
       && String.lowercase_ascii (String.sub s 0 size) = field_name
    then (String.sub s (size + 2) (String.length s - size - 2)) :: search l
    else search l in
  search
(*e: function [[Http_headers.get_multi_header]] *)

(*s: function [[Http_headers.header_type]] *)
let header_type s =
  match Str.bounded_split (regexp "[:]") s 2 with
  | [t;_] -> String.lowercase_ascii t
  | _ -> raise (Invalid_HTTP_header s)
(*e: function [[Http_headers.header_type]] *)

(*s: function [[Http_headers.merge_headers]] *)
(* Keep only unmodified headers *)
let merge_headers oldh newh =
  let rec filter acc = function
     [] -> acc
   | s::l ->
      if String.length s > 5 && String.sub s 0 5 = "HTTP/" then
       filter acc l
      else
       try
        let t = header_type s in
        let d = get_header t newh in
      filter acc l
       with
          Invalid_HTTP_header _ -> 
          Log.debug (sprintf "Dumping invalid header (%s)" s);
              filter acc l
        | Not_found -> filter (s::acc) l in
  (filter [] oldh) @ newh
(*e: function [[Http_headers.merge_headers]] *)

(*s: function [[Http_headers.remove_headers]] *)
let remove_headers hs names =
  Log.debug "remove headers";
  let rec rem acc = function
     [] -> acc
   | h::l ->
      try
       let t = header_type h in
         if List.mem t names then rem acc l
     else rem (h::acc) l
      with
        Invalid_HTTP_header s ->
      Log.debug (sprintf "Dumping invalid header (%s)" s);
          rem acc l

   in rem [] hs
(*e: function [[Http_headers.remove_headers]] *)

(*s: function [[Http_headers.status_msg]] *)
let rec status_msg = function
    [] -> raise Not_found
  | s::l -> if String.length s >= 5 (* "HTTP/" *)
          && (String.sub s 0 5) = "HTTP/"
         then (parse_status s).status_message
         else status_msg l
(*e: function [[Http_headers.status_msg]] *)


(*s: functions [[Http_headers.xxx]] [[get_header]] applications *)
let contenttype = 
  get_header "content-type"
(*x: functions [[Http_headers.xxx]] [[get_header]] applications *)
let contentlength l = 
  let h = get_header "content-length" l in
  try int_of_string h 
  with _ -> raise Not_found
(*x: functions [[Http_headers.xxx]] [[get_header]] applications *)
let contentencoding = 
  get_header "content-encoding"
(*e: functions [[Http_headers.xxx]] [[get_header]] applications *)
let location = 
  get_header "location"
let challenge = 
  get_header "www-authenticate"
let proxy_challenge = 
  get_header "proxy-authenticate"

let expires hs =
  try Some (Lexdate.ht_of_string (get_header "expires" hs))
  with
     Not_found -> None
   | _ -> Log.f ("warning: Can't parse Expires header ");
      None 

(*s: constant [[Http_headers.is_contentencoding]] *)
let is_contentencoding =
  let l = String.length "Content-Encoding" in
  (fun s ->
       String.length s >= l + 2
    && String.lowercase_ascii (String.sub s 0 (l+2)) = "content-encoding: ")
(*e: constant [[Http_headers.is_contentencoding]] *)

(*s: function [[Http_headers.rem_contentencoding]] *)
let rec rem_contentencoding = function
   [] -> []
 | h::l when is_contentencoding h -> l
 | x::l -> x :: rem_contentencoding l
(*e: function [[Http_headers.rem_contentencoding]] *)



(* 
 * Details for specific headers
 *)

(*s: type [[Http_headers.authScheme]] *)
(* Authorisation headers *)
type authScheme =
    AuthBasic
  | AuthExtend of string
(*e: type [[Http_headers.authScheme]] *)

(*s: type [[Http_headers.authChallenge]] *)
type authChallenge =
    { challenge_scheme : authScheme;
      challenge_realm : string;
      challenge_params: (string * string) list
    }
(*e: type [[Http_headers.authChallenge]] *)

(*s: type [[Http_headers.media_parameter]] *)
(* Media types *)
type media_parameter = string * string
(*e: type [[Http_headers.media_parameter]] *)
(*s: type [[Http_headers.media_type]] *)
type media_type = string * string
(*e: type [[Http_headers.media_type]] *)

(*s: type [[Http_headers.hint]] *)
(* Associating MIME type or Content-Encoding with file/URI suffix *)
type hint =
  | ContentType     of Messages.header
  | ContentEncoding of Messages.header
(*e: type [[Http_headers.hint]] *)

(*s: constant [[Http_headers.suffixes]] *)
let suffixes =
   (Hashtbl.create 101 : (string, hint) Hashtbl.t)
(*e: constant [[Http_headers.suffixes]] *)

(*s: function [[Http_headers.read_suffix_file]] *)
(* In the file, we select ContentType if there is a slash,
   ContentEncoding otherwise *)
let read_suffix_file f =
 try
   let ic = open_in f in
   try while true do
     let l = input_line ic in
     if l <> "" && l.[0] <> '#' 
     then
       let tokens = 
         split_str (function ' '|'\t' -> true | _ -> false) l in
       match tokens with
       | [] -> ()
       | x::l ->
           try 
             let _ = String.index x '/' in
             l |> List.iter  (fun sufx -> 
                Hashtbl.add suffixes sufx  (ContentType ("Content-Type: "^x))
             )
          with Not_found ->
            l |> List.iter  (fun sufx ->
                Hashtbl.add suffixes sufx  
                  (ContentEncoding ("Content-Encoding: "^x))
            )
     done
   with End_of_file -> close_in ic
 with Sys_error _ ->  ()
(*e: function [[Http_headers.read_suffix_file]] *)

(*s: constant [[Http_headers.default_hints]] *)
let default_hints = [ 
  "html",	ContentType  "Content-Type: text/html";
  "htm",	ContentType  "Content-Type: text/html";

  "txt",  	ContentType  "Content-Type: text/plain";

  "ps",  	ContentType  "Content-Type: application/postscript";
  "dvi",  	ContentType  "Content-Type: application/x-dvi";

  "gif",	ContentType  "Content-Type: image/gif";
  "jpeg",	ContentType  "Content-Type: image/jpeg";
  "jpg",	ContentType  "Content-Type: image/jpeg";
  "tiff",	ContentType  "Content-Type: image/tiff";
  "tif",	ContentType  "Content-Type: image/tiff";

  "au",		ContentType  "Content-Type: audio/basic";
  "snd",	ContentType  "Content-Type: audio/basic";
  "wav",	ContentType  "Content-Type: audio/x-wav";
  "mid",	ContentType  "Content-Type: audio/midi";

  "mpeg",	ContentType  "Content-Type: video/mpeg";
  "mpg",	ContentType  "Content-Type: video/mpeg";
  "avi",	ContentType  "Content-Type: video/avi";
  "fli",	ContentType  "Content-Type: video/fli";
  "flc",	ContentType  "Content-Type: video/fli";

   (*s: [[Http_headers.suffixes]] elements *)
   "gz",		ContentEncoding  "Content-Encoding: gzip";
   "Z",		ContentEncoding  "Content-Encoding: compress";

   "asc",	ContentEncoding  "Content-Encoding: pgp";
   "pgp",	ContentEncoding  "Content-Encoding: pgp";
   (*x: [[Http_headers.suffixes]] elements *)
   "cmo", ContentType "Content-Type: application/x-caml-applet; encoding=bytecode";
   (*e: [[Http_headers.suffixes]] elements *)
]
(*e: constant [[Http_headers.default_hints]] *)

(* Even if we don't have a suffix file... *)
(*s: toplevel [[Http_headers._1]] *)
(* If the suffix file says otherwise, it will have priority *)
let _ = 
  default_hints |> List.iter (fun (s,t) -> Hashtbl.add suffixes s t)
(*e: toplevel [[Http_headers._1]] *)

(*s: function [[Http_headers.hints]] *)
let hints path =
  (* Get the url suffix *)
  let sufx = Mstring.get_suffix path in
  try
    let v = 
      try Hashtbl.find suffixes sufx 
      with Not_found -> 
        Hashtbl.find suffixes (String.lowercase_ascii sufx)
    in
    match v with
    | ContentType t -> [t] (* good, we have a type *)
    | ContentEncoding e ->
       (* we have an encoding, but do we have a type too ? *)
       let path2 = Filename.chop_suffix path ("."^sufx) in
       let sufx2 = Mstring.get_suffix path2 in
       begin try 
         let v2 = Hashtbl.find suffixes sufx2 in
         match v2 with
         | ContentType t -> (* good, we have a type *)
            [t;e]
         | ContentEncoding _ -> [e] (* nah, forget it *)
        with Not_found -> [e] (* no type *)
      end
  with Not_found -> [] (* no hint ... *)
(*e: function [[Http_headers.hints]] *)



(*s: constant [[Http_headers.status_messages]] *)
(* Messages in Status-Line *)
let status_messages = Hashtbl.create 101
(*e: constant [[Http_headers.status_messages]] *)
(*s: toplevel [[Http_headers._2]] *)
let _ = 
  [ 200, "OK";

    201, "Created";
    202, "Accepted";
    204, "No Content";

    301, "Moved Permanently";
    302, "Moved Temporarily";
    304, "Not Modified";

    400, "Bad Request";
    401, "Unauthorized";
    403, "Forbidden";
    404, "Not Found";

    500, "Internal Server Error";
    501, "Not Implemented";
    502, "Bad Gateway";
    503, "Service Unavailable";

    (* These are proposed for HTTP1.1 *)
    407, "Proxy Authentication Required"
  ] |> List.iter (function (code, msg) -> Hashtbl.add status_messages code msg)
(*e: toplevel [[Http_headers._2]] *)

(*s: function [[Http_headers.status_message]] *)
let status_message code =  
  try Hashtbl.find status_messages code
  with Not_found -> " "
(*e: function [[Http_headers.status_message]] *)

(*s: function [[Http_headers.http_status]] *)
(* A typical status line *)
let http_status code =
  {
   status_version = "HTTP/1.0";
   status_code = code;
   status_message = status_message code
  }
(*e: function [[Http_headers.http_status]] *)


(*e: http/http_headers.ml *)

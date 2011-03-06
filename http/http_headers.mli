open Messages

exception Invalid_HTTP_header of string

val parse_status : string -> status
  (* Parses a Status-Line
     Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF
     Raises [Invalid_HTTP_header "Status-Line"] 
     or [Not_found] if the string is not a Status-Line at all *)

val parse_request : string -> request
  (* Parses a Request-Line
     Request-Line = Method SP Request-URI SP HTTP-Version CRLF
     Raises [Invalid_HTTP_header "Request-Line"] *)

val get_header : string -> header list -> string
  (* [get_header field_name hs] returns the field_value, if any, of
     the headers, or raises [Not_found].
     [field_name] is the token is lowercase (e.g. "content-type") *)

val get_multi_header : string -> header list -> string list
  (* [get_multi_header field_name hs] returns the list of field_value
     of the headers.
     [field_name] is the token is lowercase (e.g. "content-type") *)

val merge_headers : header list -> header list -> header list
  (* [merge_headers oldhs newhs] merges headers, overriding headers in
     [oldhs] by headers in [newhs] *)

val remove_headers : header list -> string list -> header list
  (* [remove_headers hs field_names] returns [hs] without the headers
     with field_name present in [field_names] *)

val header_type : string -> string
  (* [header_type h] returns the field_name token of [h], in lowercase *)


(* Predefined access functions *)
val contenttype : header list -> string
  (* Content-Type *)
val contentlength : header list -> int
  (* Content-Length *)
val contentencoding : header list -> string
  (* Content-Encoding *)
val location : header list -> string
  (* Location *)
val challenge : header list -> string
  (* WWW-Authenticate *)
val proxy_challenge : header list -> string
  (* Proxy-Authenticate *)
val expires : header list -> Http_date.http_time option
  (* Expires *)

val rem_contentencoding : header list -> header list

val status_msg : header list -> string

(* Common headers *)
val http_status : int -> status
  (* [http_status n] returns Status-Line for code [n] *)
val status_message : int -> string
  (* [status_message n] returns Reason-Phrase for code [n] *)

(* 
 * Details for specific headers
 *)

(* Authorisation headers *)
type authScheme =
    AuthBasic
  | AuthExtend of string

type authChallenge =
    { challenge_scheme : authScheme;
      challenge_realm : string;
      challenge_params: (string * string) list
    }

(* Media types *)

type media_parameter = string * string
type media_type = string * string

(* Associating MIME type or Content-Encoding with file/URI suffix *)
type hint =
    ContentType of header
  | ContentEncoding of header

val hints : string -> header list

val read_suffix_file : string -> unit

(*s: ./http/http_headers.mli *)
open Messages

(*s: exception Http_headers.Invalid_HTTP_header *)
exception Invalid_HTTP_header of string
(*e: exception Http_headers.Invalid_HTTP_header *)

(*s: signature Http_headers.parse_status *)
val parse_status : string -> Messages.status
  (* Parses a Status-Line
     Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF
     Raises [Invalid_HTTP_header "Status-Line"] 
     or [Not_found] if the string is not a Status-Line at all *)
(*e: signature Http_headers.parse_status *)

(*s: signature Http_headers.parse_request *)
val parse_request : string -> Messages.request
  (* Parses a Request-Line
     Request-Line = Method SP Request-URI SP HTTP-Version CRLF
     Raises [Invalid_HTTP_header "Request-Line"] *)
(*e: signature Http_headers.parse_request *)

(*s: signature Http_headers.get_header *)
val get_header : string -> header list -> string
  (* [get_header field_name hs] returns the field_value, if any, of
     the headers, or raises [Not_found].
     [field_name] is the token is lowercase (e.g. "content-type") *)
(*e: signature Http_headers.get_header *)

(*s: signature Http_headers.get_multi_header *)
val get_multi_header : string -> header list -> string list
  (* [get_multi_header field_name hs] returns the list of field_value
     of the headers.
     [field_name] is the token is lowercase (e.g. "content-type") *)
(*e: signature Http_headers.get_multi_header *)

(*s: signature Http_headers.merge_headers *)
val merge_headers : header list -> header list -> header list
  (* [merge_headers oldhs newhs] merges headers, overriding headers in
     [oldhs] by headers in [newhs] *)
(*e: signature Http_headers.merge_headers *)

(*s: signature Http_headers.remove_headers *)
val remove_headers : header list -> string list -> header list
  (* [remove_headers hs field_names] returns [hs] without the headers
     with field_name present in [field_names] *)
(*e: signature Http_headers.remove_headers *)

(*s: signature Http_headers.header_type *)
val header_type : string -> string
  (* [header_type h] returns the field_name token of [h], in lowercase *)
(*e: signature Http_headers.header_type *)


(*s: signature Http_headers.contenttype *)
(* Predefined access functions *)
val contenttype : header list -> string
  (* Content-Type *)
(*e: signature Http_headers.contenttype *)
(*s: signature Http_headers.contentlength *)
val contentlength : header list -> int
  (* Content-Length *)
(*e: signature Http_headers.contentlength *)
(*s: signature Http_headers.contentencoding *)
val contentencoding : header list -> string
  (* Content-Encoding *)
(*e: signature Http_headers.contentencoding *)
(*s: signature Http_headers.location *)
val location : header list -> string
  (* Location *)
(*e: signature Http_headers.location *)
(*s: signature Http_headers.challenge *)
val challenge : header list -> string
  (* WWW-Authenticate *)
(*e: signature Http_headers.challenge *)
(*s: signature Http_headers.proxy_challenge *)
val proxy_challenge : header list -> string
  (* Proxy-Authenticate *)
(*e: signature Http_headers.proxy_challenge *)
(*s: signature Http_headers.expires *)
val expires : header list -> Http_date.http_time option
  (* Expires *)
(*e: signature Http_headers.expires *)

(*s: signature Http_headers.rem_contentencoding *)
val rem_contentencoding : header list -> header list
(*e: signature Http_headers.rem_contentencoding *)

(*s: signature Http_headers.status_msg *)
val status_msg : header list -> string
(*e: signature Http_headers.status_msg *)

(*s: signature Http_headers.http_status *)
(* Common headers *)
val http_status : int -> status
  (* [http_status n] returns Status-Line for code [n] *)
(*e: signature Http_headers.http_status *)
(*s: signature Http_headers.status_message *)
val status_message : int -> string
  (* [status_message n] returns Reason-Phrase for code [n] *)
(*e: signature Http_headers.status_message *)

(* 
 * Details for specific headers
 *)

(*s: type Http_headers.authScheme *)
(* Authorisation headers *)
type authScheme =
    AuthBasic
  | AuthExtend of string
(*e: type Http_headers.authScheme *)

(*s: type Http_headers.authChallenge *)
type authChallenge =
    { challenge_scheme : authScheme;
      challenge_realm : string;
      challenge_params: (string * string) list
    }
(*e: type Http_headers.authChallenge *)

(*s: type Http_headers.media_parameter *)
(* Media types *)
type media_parameter = string * string
(*e: type Http_headers.media_parameter *)
(*s: type Http_headers.media_type *)
type media_type = string * string
(*e: type Http_headers.media_type *)

(*s: type Http_headers.hint *)
(* Associating MIME type or Content-Encoding with file/URI suffix *)
type hint =
  | ContentType     of header
  | ContentEncoding of header
(*e: type Http_headers.hint *)

(*s: signature Http_headers.hints *)
val hints : string -> header list
(*e: signature Http_headers.hints *)

(*s: signature Http_headers.read_suffix_file *)
val read_suffix_file : string -> unit
(*e: signature Http_headers.read_suffix_file *)
(*e: ./http/http_headers.mli *)

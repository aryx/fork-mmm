(*s: ./http/lexheaders.mli *)
open Http_headers

(*s: signature Lexheaders.media_type *)
val media_type : 
  string -> media_type * Http_headers.media_parameter list
(*e: signature Lexheaders.media_type *)
(*s: signature Lexheaders.challenge *)
val challenge : Lexing.lexbuf -> authChallenge
(*e: signature Lexheaders.challenge *)

(*e: ./http/lexheaders.mli *)

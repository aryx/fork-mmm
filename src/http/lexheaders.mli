(*s: http/lexheaders.mli *)

(*s: signature [[Lexheaders.media_type]] *)
val media_type : 
  string -> Http_headers.media_type * Http_headers.media_parameter list
(*e: signature [[Lexheaders.media_type]] *)
(*s: signature [[Lexheaders.challenge]] *)
val challenge : Lexing.lexbuf -> Http_headers.authChallenge
(*e: signature [[Lexheaders.challenge]] *)

(*e: http/lexheaders.mli *)

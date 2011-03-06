open Http_headers

val media_type : string -> media_type * media_parameter list
val challenge : Lexing.lexbuf -> authChallenge


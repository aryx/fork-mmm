val f : Lexing.lexbuf -> Url.t
val make : string -> Url.t
   (* raise Url_Lexing(msg,pos) *)
val maken : string -> Url.t
   (* raise Url_Lexing(msg,pos) *)

val remove_dots : string -> string
val normalize : string -> string

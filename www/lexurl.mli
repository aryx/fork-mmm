(*s: ./www/lexurl.mli *)
(*s: signature Lexurl.f *)
val f : Lexing.lexbuf -> Url.t
(*e: signature Lexurl.f *)
(*s: signature Lexurl.make *)
val make : string -> Url.t
   (* raise Url_Lexing(msg,pos) *)
(*e: signature Lexurl.make *)
(*s: signature Lexurl.maken *)
val maken : string -> Url.t
   (* raise Url_Lexing(msg,pos) *)
(*e: signature Lexurl.maken *)

(*s: signature Lexurl.remove_dots *)
val remove_dots : string -> string
(*e: signature Lexurl.remove_dots *)
(*s: signature Lexurl.normalize *)
val normalize : string -> string
(*e: signature Lexurl.normalize *)
(*e: ./www/lexurl.mli *)

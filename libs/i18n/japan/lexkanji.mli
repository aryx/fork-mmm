exception MustbeEUC
exception MustbeSJIS
exception Lexkanji_Error of string

type lexdata = { 
    stored: Wstream.t;
    unresolved: Ebuffer.t
}

val newdata : unit -> lexdata

val ascii : Lexing.lexbuf -> lexdata -> unit
val iso8859 : Lexing.lexbuf -> lexdata -> unit
val junet : Lexing.lexbuf -> lexdata -> unit
val eucjapan : Lexing.lexbuf -> lexdata -> unit
val sjis : Lexing.lexbuf -> lexdata -> unit
val eucorsjis : Lexing.lexbuf -> lexdata -> unit
val remove_garbage : Lexing.lexbuf -> lexdata -> unit

val length : string -> int

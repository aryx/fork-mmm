(*s: ./html/lexhtml.mli *)
(*s: signature Lexhtml.strict *)
val strict : bool ref
    (* if true, use strict parsing; else, activate leniency on some
       lexing decisisons such as: comments, attribute names and values
     *)
(*e: signature Lexhtml.strict *)

type t
(*s: signature Lexhtml.new_data *)
val new_data : unit -> t
    (* instance data for a lexer; must be allocated for each instance, in
       order to get reentrant lexers
     *)
(*e: signature Lexhtml.new_data *)

(*s: enum Lexhtml.warnings *)
type warnings = (string * int) list
(*e: enum Lexhtml.warnings *)

(*s: signature Lexhtml.html *)
val html : Lexing.lexbuf -> t -> warnings * Html.token * Html.location
(*e: signature Lexhtml.html *)
(*s: signature Lexhtml.cdata *)
val cdata : Lexing.lexbuf -> t -> warnings * Html.token * Html.location
(*e: signature Lexhtml.cdata *)
(*e: ./html/lexhtml.mli *)

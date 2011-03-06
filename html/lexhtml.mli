val strict : bool ref
    (* if true, use strict parsing; else, activate leniency on some
       lexing decisisons such as: comments, attribute names and values
     *)

type t
val new_data : unit -> t
    (* instance data for a lexer; must be allocated for each instance, in
       order to get reentrant lexers
     *)

type warnings = (string * int) list

val html : Lexing.lexbuf -> t -> warnings * Html.token * Html.location
val cdata : Lexing.lexbuf -> t -> warnings * Html.token * Html.location

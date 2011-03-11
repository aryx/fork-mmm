open Fonts

(*
 * Definition of attributes of symbolic fonts (font-modifiers)
 *)

val init : string -> string -> unit
    (* [init family slant] *)

val set_font : string -> fontAttrs -> unit
    (* [set_font symbolic_name attrs] *)
val get_font : string -> fontAttrs
    (* [get_font symbolic_name] *)

(*
 * Retrieves graphical attributes for a given font
 *)

val get : string -> Htmlfmt.gattr list

(*s: ./display/styles.mli *)
open Fonts

(*s: signature Styles.init *)
(*
 * Definition of attributes of symbolic fonts (font-modifiers)
 *)

val init : string -> string -> unit
    (* [init family slant] *)
(*e: signature Styles.init *)

(*s: signature Styles.set_font *)
val set_font : string -> fontAttrs -> unit
    (* [set_font symbolic_name attrs] *)
(*e: signature Styles.set_font *)
(*s: signature Styles.get_font *)
val get_font : string -> fontAttrs
    (* [get_font symbolic_name] *)
(*e: signature Styles.get_font *)

(*s: signature Styles.get *)
(*
 * Retrieves graphical attributes for a given font
 *)

val get : string -> Htmlfmt.gattr list
(*e: signature Styles.get *)
(*e: ./display/styles.mli *)

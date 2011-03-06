type fontInfo =
   Family of string
 | Weight of string
 | Slant of string
 | FontIndex of int
 | FontDelta of int

type fontAttrs = fontInfo list

val print_fontAttrs: fontAttrs -> unit

(* type fontDesc *)

type fontDesc =
  { pattern: Jpf_font.pattern;
    mutable pxlsz : int }

val default : fontDesc ref
val merge : fontDesc -> fontAttrs -> fontDesc
val compute_tag : fontDesc -> string * Tk.options list

val font_index : int -> int
val pxlsz : int -> int

(*-*)
val default_sizes : string list
val reset : unit -> unit

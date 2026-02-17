(*s: display/fonts.mli *)

(*s: type [[Fonts.fontInfo]] *)
type fontInfo =
  | Family of string
  | Weight of string
  | Slant of string
  | FontIndex of int
  | FontDelta of int
(*e: type [[Fonts.fontInfo]] *)

(*s: type [[Fonts.fontAttrs]] *)
type fontAttrs = fontInfo list
(*e: type [[Fonts.fontAttrs]] *)

(*s: type [[Fonts.fontDesc]] *)
type fontDesc = { 
  pattern: Jpf_font.pattern;
  mutable pxlsz: int;
}
(*e: type [[Fonts.fontDesc]] *)

(*s: signature [[Fonts.default]] *)
val default: fontDesc ref
(*e: signature [[Fonts.default]] *)

(*s: signature [[Fonts.print_fontAttrs]] *)
val print_fontAttrs: fontAttrs -> unit
(*e: signature [[Fonts.print_fontAttrs]] *)

(*s: signature [[Fonts.merge]] *)
val merge: fontDesc -> fontAttrs -> fontDesc
(*e: signature [[Fonts.merge]] *)
(*s: signature [[Fonts.compute_tag]] *)
val compute_tag: fontDesc -> string * Tk.options list
(*e: signature [[Fonts.compute_tag]] *)

(*s: signature [[Fonts.font_index]] *)
val font_index: int -> int
(*e: signature [[Fonts.font_index]] *)
(*s: signature [[Fonts.pxlsz]] *)
val pxlsz: int -> int
(*e: signature [[Fonts.pxlsz]] *)

(*s: signature [[Fonts.default_sizes]] *)
(*-*)
val default_sizes: string list
(*e: signature [[Fonts.default_sizes]] *)

(*s: signature [[Fonts.reset]] *)
val reset: unit -> unit
(*e: signature [[Fonts.reset]] *)
(*e: display/fonts.mli *)

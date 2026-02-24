(*s: display/fonts.ml *)
open I18n

open Tk
open Jpf_font

(*s: type Fonts.fontDesc (./display/fonts.ml) *)
(* Font manipulation *)

type fontDesc = 
    { pattern: Jpf_font.pattern;
      mutable pxlsz: int (* not pxlsz, but font index *)
    }
(*e: type Fonts.fontDesc (./display/fonts.ml) *)

(*s: type Fonts.fontInfo (./display/fonts.ml) *)
type fontInfo =
   Family of string
 | Weight of string
 | Slant of string
 | FontIndex of int
 | FontDelta of int
(*e: type Fonts.fontInfo (./display/fonts.ml) *)

(*s: type Fonts.fontAttrs (./display/fonts.ml) *)
type fontAttrs = fontInfo list
(*e: type Fonts.fontAttrs (./display/fonts.ml) *)

(*s: function [[Fonts.copy]] *)
let copy fd = { pattern= Jpf_font.copy fd.pattern;
        pxlsz= fd.pxlsz }
(*e: function [[Fonts.copy]] *)

(*s: function [[Fonts.print_fontAttrs]] *)
let print_fontAttrs attrs =
  List.iter (function 
    Family s -> prerr_string ("family: " ^ s ^ " ")
  | Weight s -> prerr_string ("weight: " ^ s ^ " ")
  | Slant s -> prerr_string ("slant: " ^ s ^ " ")
  | FontIndex i -> prerr_string ("index: " ^ string_of_int i ^ " ")
  | FontDelta i -> prerr_string ("delta: " ^ string_of_int i ^ " ")) attrs;
  prerr_endline "";
(*e: function [[Fonts.print_fontAttrs]] *)
;;

(*s: function [[Fonts.merge]] *)
(* Merge font attributes in a fontDesc *)
let merge fd fil =
  let newfd = copy fd in
  List.iter (function
      Family s -> newfd.pattern.family <- Some s
    | Weight s -> newfd.pattern.weight <- Some s
    | Slant s -> newfd.pattern.slant <- Some s
    | FontIndex i -> newfd.pxlsz <- i
    | FontDelta n -> newfd.pxlsz <- newfd.pxlsz + n
    )
    fil;
  newfd
(*e: function [[Fonts.merge]] *)

(* HTML3.2 specifies that absolute font size are ranging from 1 to 7, 
   the default basefont, used for "normal" text, being 3.

   The preference settings allow:
    - definition of list of pixel sizes
    - definition of default size   
    - definition of header size.

   To compute the HTML fonts [1..7], we look for the default
   pixel size in the list of the given sizes: this defines the 
   default base (3).

   We map these sizes to X Font Pxlsz, keeping some latitude for
   mapping the base. The lowest reasonable font is 8
 *)

(*s: constant [[Fonts.default_sizes]] *)
(* List of authorized pixel sizes *)
let default_sizes = ["8"; "10"; "12"; "14"; "15"; "16"; "18"; "20"; "24"; "26"; "28"]
(*e: constant [[Fonts.default_sizes]] *)
(*s: constant [[Fonts.sizes]] *)
let sizes = ref (Array.of_list (List.map int_of_string default_sizes))
(*e: constant [[Fonts.sizes]] *)

(*s: function [[Fonts.get_index]] *)
(* Given a size in pixels, find out the corresponding index array 
   (which is the max of defined sized lower than argument)
 *)
let get_index size =
  let len = Array.length !sizes in
  let rec walk n =
    if n >= len then len - 1
    else if !sizes.(n) > size then n-1
    else walk (succ n)
  in 
  let idx = walk 0 in
  if idx < 0 then 0 else idx
(*e: function [[Fonts.get_index]] *)


(*s: constant [[Fonts.base_index]] *)
let base_index = ref (get_index 15)
(*e: constant [[Fonts.base_index]] *)

(*s: function [[Fonts.font_index]] *)
(* Convert a pxlsz to an absolute font 
 * (the base_index is always the absolute font 3 byte defintion of HTML)
 *)
let font_index pxlsz =
  (get_index pxlsz) - (!base_index - 3)
(*e: function [[Fonts.font_index]] *)

(*s: function [[Fonts.pxlsz]] *)
(* Convert an absolute font to a pxlsz *)
let pxlsz absfont =
  let font_idx = absfont + (!base_index - 3) in
  let safe_idx = 
    if font_idx < 0 then 0
    else if font_idx >= Array.length !sizes then Array.length !sizes - 1
    else font_idx in
   !sizes.(safe_idx)
(*e: function [[Fonts.pxlsz]] *)

(*s: constant [[Fonts.tags]] *)
(*
 * Tag names for fonts (this table is shared by all widgets)
 * We share tags for fonts, but this requires combinations of all
 * possible styles (weight, slant and size). The tag attribute is computed 
 * on demand. Each widget must do its "tag configure" separately, since these
 * are not shared by all text widgets (even in a same class)
 *)


let tags = Hashtbl.create 37
(*e: constant [[Fonts.tags]] *)

(*s: constant [[Fonts.default]] *)
let default = ref 
    { pattern= Jpf_font.copy Jpf_font.empty_pattern;
      pxlsz = 3 }
(*e: constant [[Fonts.default]] *)

(*s: function [[Fonts.compute_tag]] *)
(* For a given fontDesc, return the name of the tags and its attributes *)
let rec compute_tag fd =
  let font_key = 
    Jpf_font.string_of_pattern fd.pattern ^ string_of_int fd.pxlsz 
  in
  try 
    Hashtbl.find tags font_key
  with Not_found ->
    let tagdesc =
      let pxlsz = pxlsz fd.pxlsz in
      let pattern = {fd.pattern with pixelSize= Some pxlsz} in
      try
        let display = 
          match Protocol.default_display () with
            "" -> None | x -> Some x
        in
        let fontid, fontname =
              (* find latin font *)
          let xlfd = Jpf_font.nearest_pixel_size display true pattern in
          let latin_f = Jpf_font.string_of_valid_xlfd xlfd in
          xlfd.family^xlfd.weight^xlfd.slant^(string_of_int pxlsz), latin_f
        in
    
        fontid, [Font fontname]
      with (* Invalid_argument f *) _ ->  (* font is not available *)
        Log.f (s_ "Font for %s is not available" 
                 (Jpf_font.string_of_pattern pattern));
        if fd = !default 
        then ("fixedfont", [Font "fixed"])
        else compute_tag !default
     in
     Hashtbl.add tags font_key tagdesc;
     tagdesc
(*e: function [[Fonts.compute_tag]] *)

(*s: function [[Fonts.reset]] *)
(* Mapping with preferences : *fontPixels is also used to define our array 
 * We use a mute preference handler to synchronize : this handler is called
 * after the loading of the resource file.
 *)
let reset () =
  Hashtbl.clear tags; (* since tags use font index *)
  let l = Tkresource.stringlist "fontPixels" default_sizes in
  sizes := Array.of_list (List.map (fun x -> 
    try 
      int_of_string x 
    with 
      e -> 
    Log.f ("Fonts.reset error for size "^x); 
    raise e) l);
  (* now we need to compute the base (we need to know some of Prefs internal)*)
  let b = Tkresource.string "prefDefaultFont" "" in
  if b = "" then base_index := get_index 15
  else
    let tokens = Mstring.split_str (fun c -> c='-') b in
    base_index := get_index (
       try int_of_string (List.nth tokens 6)
       with Failure "int_of_string" | Failure "nth" -> 15)
(*e: function [[Fonts.reset]] *)
(*e: display/fonts.ml *)

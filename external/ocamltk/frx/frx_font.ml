open Tk
open Widget

let version = "$Id: frx_font.ml,v 1.1 1996/10/22 15:55:31 rouaix Exp $"

(* 
 * Finding fonts. Inspired by code in Ical by Sanjay Ghemawat.
 * Possibly bogus because some families use "i" for italic where others
 * use "o".
 * wght: bold, medium
 * slant: i, o, r
 * pxlsz: 8, 10, ...
*)
module StringSet = Set.Make(struct type t = string let compare = compare end)

let available_fonts = ref (StringSet.empty)

let get_canvas = 
  Frx_misc.autodef (fun () -> Canvas.create Widget.default_toplevel [])


let find fmly wght slant pxlsz =
  let fontspec =
     "-*-"^fmly^"-"^wght^"-"^slant^"-normal-*-"^string_of_int pxlsz^"-*-*-*-*-*-iso8859-1" in
    if StringSet.mem fontspec !available_fonts then fontspec
    else
      let c = get_canvas() in
      try
	let tag = Canvas.create_text c (Pixels 0) (Pixels 0) 
                                [Text "foo"; Font fontspec] in
	   Canvas.delete c [tag];
	   available_fonts := StringSet.add fontspec !available_fonts;
	   fontspec
      with
        _ -> raise (Invalid_argument fontspec)


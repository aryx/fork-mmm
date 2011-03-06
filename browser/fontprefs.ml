open Printf
open Tk
(* Specify set of attributes of a font *)
(* family, weight, slant, pxlsz *)
(* We use a font string, and select only the relevant components *)
(* 
-fndry-fmly-wght-slant-sWdth-adstyl-pxlsz-ptSz-resx-resy-spc-avgWdth-reg-enc
   0    1    2    3     4      5     6     7    8    9   10    11     12  13
*)
open Fonts

let fontspec2attrs s =
  let tokens = Mstring.split_str (fun c -> c='-') s in
  if List.length tokens <> 14 then
    failwith ("incomplete font specification: " ^ s)
  else (* should not fail *)
    let attrs = ref [] in
      (match List.nth tokens 1 
       with "*" -> () | s -> attrs := (Family s) :: !attrs);
      (match List.nth tokens 2 
       with "*" -> () | s -> attrs := (Weight s) :: !attrs);
      (match List.nth tokens 3 
       with "*" -> () | s -> attrs := (Slant s) :: !attrs);
      (match List.nth tokens 6 with
         "*" -> () 
	| s -> try
	         attrs := (FontIndex (font_index (int_of_string s))) :: !attrs
	       with Failure "int_of_string" ->
		 failwith ("pxlsz not an integer: " ^ s));
      !attrs

let attrs2fontspec l =
  let rec family = function
     [] -> "*"
   | (Family s)::_ -> s
   | x::l -> family l
  and weight = function
     [] -> "*"
   | (Weight s)::_ -> s
   | x::l -> weight l
  and slant = function
     [] -> "*"
   | (Slant s)::_ -> s
   | x::l -> slant l
  and pxlsz = function
     [] -> "*"
   | (FontIndex s)::_ -> string_of_int (Fonts.pxlsz s)
   | x::l -> pxlsz l in

  sprintf "-*-%s-%s-%s-normal-*-%s-*-*-*-*-*-iso8859-1"
          (family l) (weight l) (slant l) (pxlsz l)

(* Build a family menu *)
let default_families = 
  ["courier"; "helvetica"; "lucida"; "new century schoolbook";
   "times"; "fixed"; "*"]
let families () =
 Tkresource.stringlist "fontFamilies" default_families

let family_select top v = 
  Optionmenu.create  top v (families())

(* Build a weight menu *)
let default_weights = ["bold"; "medium"; "*"]
let weights () =
  Tkresource.stringlist "fontWeights" default_weights
let weight_select top v =
  Optionmenu.create top v (weights())

(* Build a slant menu *)
let default_slants = ["r"; "i"; "o"; "*"]
let slants () = 
  Tkresource.stringlist "fontSlants" default_slants
let slant_select top v =
  Optionmenu.create top v (slants())

(* Build a pixel size menu *)
let pixels() =
  Tkresource.stringlist "fontPixels" Fonts.default_sizes
let pixels_select top v = 
  Optionmenu.create top v (pixels())


(* fontspecv is the variable used for the full X font name; it is used
 * internally (and for saving the prefs), and must be maintained consistent
 * with the displayed state.
 *   - initialisation time : 
 *      given the attributes, write the X name in the variable
 *   - edition time:
 *      electric update of the variable and the styles
 *)

let font_select top getattrs setattrs =
  let familyv = Textvariable.create_temporary top
  and weightv = Textvariable.create_temporary top
  and slantv = Textvariable.create_temporary top
  and pixelsv = Textvariable.create_temporary top
  and fontspecv = Textvariable.create_temporary top
  in
  let f = Frame.create top [] in
  let buttons = 
    List.map2 (fun create v -> 
                 Textvariable.set v "*"; 
                 let x,_ = create f v in x) 
              [family_select; weight_select; slant_select; pixels_select]
              [familyv; weightv; slantv; pixelsv] in
  pack buttons [Side Side_Left];
  (* electric updates 
   * Whenever one of the attributes changes, we must change the fontspec
   * and possibly recompute the attributes
   *)
  let setv _ =
    let font = sprintf "-*-%s-%s-%s-normal-*-%s-*-*-*-*-*-iso8859-1"
        (Textvariable.get familyv)
	(Textvariable.get weightv)
	(Textvariable.get slantv)
        (Textvariable.get pixelsv)
    in 
    let attrs = fontspec2attrs font in
    Textvariable.set fontspecv font;
    setattrs attrs
  in
  List.iter (fun v ->
    let rec el () = Textvariable.handle v (fun () -> setv(); el()) in el())
    [familyv; weightv; slantv; pixelsv];
  
  (* initialisation from memory (v=fontspecv) *)
  let init_pref v =
    let attrs = getattrs() in
    (* Set all variables; electric update does the rest *)
    List.iter (function
        Family s -> Textvariable.set familyv s
      | Weight s -> Textvariable.set weightv s
      | Slant s -> Textvariable.set slantv s
      | FontIndex s ->
          Textvariable.set pixelsv (string_of_int (Fonts.pxlsz s))
      |	_ -> assert false)
      attrs
  (* initialisation from loaded strings (v=fontspecv) *)
  and set_pref v =
    let font = Textvariable.get v in
    let attrs = fontspec2attrs font in
    (* Set all variables; electric update rewrites everything (duh) *)
    List.iter (function
        Family s -> Textvariable.set familyv s
      | Weight s -> Textvariable.set weightv s
      | Slant s -> Textvariable.set slantv s
      | FontIndex s ->
          Textvariable.set pixelsv (string_of_int (Fonts.pxlsz s))
      |	_ -> assert false)
      attrs
  in
  f, fontspecv, init_pref, set_pref

    


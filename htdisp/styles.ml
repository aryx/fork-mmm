(* Styles are common display attributes *)
open Htmlfmt
open Fonts


(* Definition of font attributes *)
let fonttable = (Hashtbl.create 37 : (string, fontAttrs) Hashtbl.t)

let get_font =  Hashtbl.find fonttable
let set_font name attrs =
  Hashtbl.remove fonttable name;
  Hashtbl.add fonttable name attrs;
  if name = "default" then 
    Fonts.default := Fonts.merge !Fonts.default attrs


(* 
 * Graphical attributes for a given symbolic name 
 * TODO: to support a notion of style sheet, this table should be
 * specific to each display machine, and should define all the properties
 * of the style sheet display model
 *)
let table = (Hashtbl.create 37 : (string, gattr list) Hashtbl.t)

(* Merge font attributes and other attributes *)
let get s = 
  let fontattrs = 
   try Hashtbl.find fonttable s with Not_found -> []
  and otherattrs =
   try Hashtbl.find table s with Not_found -> []
  in
  let attrs =  List.map (fun fi -> Font fi) fontattrs @ otherattrs 
  in
   if attrs = [] then raise Not_found else attrs

let define_style name attrs =
  Hashtbl.remove table name;
  Hashtbl.add table name attrs

let init family slant =
  Hashtbl.clear fonttable;
  Hashtbl.clear table;
  (* font initialisation is moot if we have preferences,
     but just in case (no preference file at all), we keep it*)
  List.iter (function (name,attrs) -> set_font name attrs)
    [ "default",  [Family family;  Weight "medium"; Slant "r";  FontIndex 3];
      "header1", [Family family;  Weight "bold"; Slant "r"; FontIndex 7];
      "header2", [Family family;  Weight "bold"; Slant "r"; FontIndex 6];
      "header3", [Family family;  Weight "medium"; Slant slant; FontIndex 5];
      "header4", [Family family;  Weight "bold"; Slant "r"; FontIndex 4];
      "header5", [Family family;  Weight "medium"; Slant slant; FontIndex 4];
      "header6", [Family family;  Weight "bold"; Slant "r"; FontIndex 4];
      "bold", [ Weight "bold"];
      "italic", [ Slant slant];
      (* should be a fixed font. Since we have newlines, spacing should be 0 *)
      "verbatim", [Family "courier"];
      "fixed", [Family "courier"]
    ];
  List.iter (function (name,attrs) -> define_style name attrs)
    [ "default", [Justification "center"; Spacing 2];
      "verbatim", [Spacing 1];
      "header1", [Justification "center"; Spacing 20];
      "header2", [Justification "center"; Spacing 10];
      "header3", [Justification "left"; Spacing 10];
      "header4", [Justification "left"; Spacing 5];
      "header5", [Justification "left"];
      "header6", [Justification "left"]
    ]


let _ = init "helvetica" "o"

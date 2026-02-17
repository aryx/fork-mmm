(*s: html/html.ml *)
open Printf

(*s: type [[Html.attribute_name]] *)
(* HTML tokens *)
type attribute_name = string 
(*e: type [[Html.attribute_name]] *)
(*s: type [[Html.attribute_value]] *)
type attribute_value = string
(*e: type [[Html.attribute_value]] *)
(*s: type [[Html.attributes]] *)
type attributes = (attribute_name * attribute_value) list
(*e: type [[Html.attributes]] *)

(*s: type [[Html.tag]] *)
type tag = {
  tag_name : string;
  attributes: attributes
}
(*e: type [[Html.tag]] *)


(*s: type [[Html.token]] *)
type token =
 | Doctype of string

 | OpenTag of tag
 | CloseTag of string

 | PCData of string
 | CData of string

 | Comment of string

 | EOF
(*e: type [[Html.token]] *)

(*s: type [[Html.location]] *)
type location = Loc of int * int
(*e: type [[Html.location]] *)

(*s: exception [[Html.Html_Lexing]] *)
exception Html_Lexing of string * int
(*e: exception [[Html.Html_Lexing]] *)
(*s: exception [[Html.Invalid_Html]] *)
exception Invalid_Html of string
(*e: exception [[Html.Invalid_Html]] *)

(*s: constant [[Html.verbose]] *)
let verbose = ref false
(*e: constant [[Html.verbose]] *)

(*s: function [[Html.warning]] *)
let warning s (Loc(n,m)) = 
  if !verbose then begin 
    eprintf "HTML Warning: %s at (%d, %d)\n" s n m;
    flush stderr
   end
(*e: function [[Html.warning]] *)


(*s: function [[Html.print]] *)
let print = function
    PCData s -> eprintf "PCData: %s\n" s
  | CData s -> eprintf "CData: %s\n" s
  | OpenTag {tag_name = n; attributes = l} ->
            eprintf "Open: %s\n" n;
         List.iter (function (a,v) ->
                   eprintf "%s=%s\n" a v) l
  | CloseTag n -> eprintf "Close: %s\n" n
  | Comment s -> eprintf "Comment: %s\n" s
  | Doctype s -> eprintf "Doctype: %s\n" s
  | EOF -> eprintf "EOF\n"
(*e: function [[Html.print]] *)

(*s: function [[Html.beautify]] *)
(*
 * Remove sequences of white
 *   turns out to be faster than global_replace in libstr
 *   could use String.blit to avoid char copying
 * NOTE: add \0 detection here (we need it for Tk)
 *)
let beautify remove_leading (s : string) =
  let s2 = Bytes.of_string s in
  let j = ref 0 in
  let white = ref remove_leading in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | ' ' | '\t' | '\r' | '\n' | '\000' -> 
       if not !white 
       then begin
         Bytes.set s2 !j ' '; 
         incr j; 
         white := true
       end
    | c -> 
       Bytes.set s2 !j c; 
       white := false; 
       incr j
  done;
  Bytes.sub_string s2 0 !j
(*e: function [[Html.beautify]] *)

(*s: function [[Html.beautify2]] *)
(* Remove also trailing space. Used for OPTION tags and TITLE *)
let beautify2 s =
  let s1 = beautify true s in
   match String.length s1 with
     0 | 1 -> s1
   | n -> if s1.[n-1] = ' ' then String.sub s1 0 (n-1) else s1
(*e: function [[Html.beautify2]] *)


(*s: function [[Html.issp]] *)
(* Is SP: when a PCData is only spaces, we skip it *)
let issp s =
  try
    for i = 0 to String.length s - 1 do
      match s.[i] with 
       ' '|'\t'|'\r'|'\n'|'\000' -> ()
      | _ -> failwith "subliminal"
    done;
    true
  with
    Failure "subliminal" -> false
(*e: function [[Html.issp]] *)
  
(*s: constant [[Html.ampersand_table]] *)
(* 
 * HTML encoding of ISO-latin1 characters
 *  cf Appendix B - Proposed Entities
 *)

let ampersand_table = 
  (Hashtbl.create 101: (string , string) Hashtbl.t)
(*e: constant [[Html.ampersand_table]] *)

(*s: constant [[Html.latin1_normal]] *)
let latin1_normal = [
  "amp", 	"&";
  "gt", 	">";
  "lt" , 	"<";
  "quot", 	"\"";

  (*s: [[latin1_normal]] elements *)
  "nbsp", 	"\160"; (* non-breaking space *)
  "iexcl",	"\161"; (* inverted exclamation mark *)
  "cent", 	"\162"; (* cent sign*)
  "pound",	"\163"; (* pound sterling sign*)
  "curren",	"\164"; (* general currency sign*)
  "yen",	"\165"; (* yen sign*)
  "brvbar",	"\166"; (* broken (vertical) bar *)
  "sect",	"\167"; (* section sign *)
  "uml",	"\168"; (* umlaut (dieresis) *)
  "copy",	"\169"; (* copyright sign *)
  "ordf",	"\170"; (* ordinal indicator, feminine *)
  "laquo",	"\171"; (* angle quotation mark, left *)
  "not",	"\172"; (* not sign *)
  "shy",	"\173"; (* soft hyphen *)
  "reg",	"\174"; (* registered sign *)
  "macr",	"\175"; (* macron *)
  "deg",	"\176"; (* degree sign *)
  "plusmn",	"\177"; (* plus-or-minus sign *)
  "sup2",	"\178"; (* superscript two *)
  "sup3",	"\179"; (* superscript three *)
  "acute",	"\180"; (* acute accent *)
  "micro",	"\181"; (* micro sign *)
  "para",	"\182"; (* pilcrow (paragraph sign) *)
  "middot",	"\183"; (* middle dot *)
  "cedil",	"\184"; (* cedilla *)
  "sup1",	"\185"; (* superscript one *)
  "ordm",	"\186"; (* ordinal indicator, masculine *)
  "raquo",	"\187"; (* angle quotation mark, right *)
  "frac14",	"\188"; (*  fraction one-quarter *)
  "frac12",	"\189"; (*  fraction one-half *)
  "frac34",	"\190"; (*  fraction three-quarters *)
  "iquest",	"\191"; (*  inverted question mark *)
  "Agrave", 	"\192";	(*  capital A, grave accent *)
  "Aacute", 	"\193";	(*  capital A, acute accent *)
  "Acirc", 	"\194";	(*  capital A, circumflex accent *)
  "Atilde", 	"\195";	(*  capital A, tilde *)
  "Auml", 	"\196";	(*  capital A, dieresis or umlaut mark *)
  "Aring", 	"\197";	(*  capital A, ring *)
  "AElig", 	"\198";	(*  capital AE diphthong (ligature) *)
  "Ccedil", 	"\199";	(*  capital C, cedilla *)
  "Egrave", 	"\200";	(*  capital E, grave accent *)
  "Eacute", 	"\201";	(*  capital E, acute accent *)
  "Ecirc", 	"\202";	(*  capital E, circumflex accent *)
  "Euml", 	"\203";	(*  capital E, dieresis or umlaut mark *)
  "Igrave", 	"\204";	(*  capital I, grave accent *)
  "Iacute", 	"\205";	(*  capital I, acute accent *)
  "Icirc", 	"\206";	(*  capital I, circumflex accent *)
  "Iuml", 	"\207";	(*  capital I, dieresis or umlaut mark *)
  "ETH", 	"\208";	(* capital Eth, Icelandic *)
  "Ntilde", 	"\209";	(*  capital N, tilde *)
  "Ograve", 	"\210";	(*  capital O, grave accent *)
  "Oacute", 	"\211";	(*  capital O, acute accent *)
  "Ocirc", 	"\212";	(*  capital O, circumflex accent *)
  "Otilde", 	"\213";	(*  capital O, tilde *)
  "Ouml", 	"\214";	(*  capital O, dieresis or umlaut mark *)
  "times",	"\215"; (*  multiply sign*)
  "Oslash", 	"\216";	(*  capital O, slash *)
  "Ugrave", 	"\217";	(*  capital U, grave accent *)
  "Uacute", 	"\218";	(*  capital U, acute accent *)
  "Ucirc", 	"\219";	(*  capital U, circumflex accent *)
  "Uuml", 	"\220";	(*  capital U, dieresis or umlaut mark *)
  "Yacute",	"\221"; (*  capital Y, acute accent *)
  "THORN", 	"\222";	(*  capital THORN, Icelandic *)
  "szlig", 	"\223";	(*  small sharp s, German (sz ligature) *)
  "agrave", 	"\224";	(*  small a, grave accent *)
  "aacute", 	"\225";	(*  small a, acute accent *)
  "acirc", 	"\226";	(*  small a, circumflex accent *)
  "atilde", 	"\227";	(*  small a, tilde *)
  "auml", 	"\228";	(*  small a, dieresis or umlaut mark *)
  "aring", 	"\229";	(*  small a, ring *)
  "aelig", 	"\230";	(*  small ae diphthong (ligature) *)
  "ccedil", 	"\231";	(*  small c, cedilla *)
  "egrave", 	"\232";	(*  small e, grave accent *)
  "eacute", 	"\233";	(*  small e, acute accent *)
  "ecirc", 	"\234";	(*  small e, circumflex accent *)
  "euml", 	"\235";	(*  small e, dieresis or umlaut mark *)
  "igrave", 	"\236";	(*  small i, grave accent *)
  "iacute", 	"\237";	(*  small i, acute accent *)
  "icirc", 	"\238";	(*  small i, circumflex accent *)
  "iuml", 	"\239";	(*  small i, dieresis or umlaut mark *)
  "eth", 	"\240";	(* small th, Icelandic *)
  "ntilde", 	"\241";	(*  small n, tilde *)
  "ograve", 	"\242";	(*  small o, grave accent *)
  "oacute", 	"\243";	(*  small o, acute accent *)
  "ocirc", 	"\244";	(*  small o, circumflex accent *)
  "otilde", 	"\245";	(*  small o, tilde *)
  "ouml", 	"\246";	(*  small o, dieresis or umlaut mark *)
  "divide",	"\247"; (*  divide sign *)
  "oslash", 	"\248";	(*  small o, slash *)
  "ugrave", 	"\249";	(*  small u, grave accent *)
  "uacute", 	"\250";	(*  small u, acute accent *)
  "ucirc", 	"\251";	(*  small u, circumflex accent *)
  "uuml", 	"\252";	(*  small u, dieresis or umlaut mark *)
  "yacute", 	"\253";	(*  small y, acute accent *)
  "thorn", 	"\254";	(*  small thorn, Icelandic *)
  "yuml", 	"\255" 	(*  small y, dieresis or umlaut mark *)
  (*e: [[latin1_normal]] elements *)
]
(*e: constant [[Html.latin1_normal]] *)

(*s: function [[Html.init]] *)
let init _lang =
  latin1_normal |> List.iter (fun (str, c) -> 
    Hashtbl.add ampersand_table str c) 
(*e: function [[Html.init]] *)

(*s: constant [[Html.get_entity]] *)
let get_entity = Hashtbl.find ampersand_table
(*e: constant [[Html.get_entity]] *)


(*s: constant [[Html.default_attributes]] *)
(* Attribute values *)
let default_attributes = [ 
  ("isindex"  , "prompt" ),  "Document is indexed/searchable: ";
                             
  ("a"        , "methods"),  "GET";     (* <A METHODS=GET> *)
  ("embed"    , "methods"),  "GET";		(* <EMBED METHODS=GET> *)
  ("embed"    , "alt"    ),  "[EMBEDDED OBJECT]";(* <EMBED ALT="EMBEDDED OBJECT"> *)
  ("form"     , "method" ),  "GET";		(* <FORM METHOD=GET> *)
  ("form"     , "enctype"),  "application/x-www-form-urlencoded";

  ("ol"       , "type"   ),  "1";       (* <OL TYPE=1 *)
  ("input"    , "type"   ),  "TEXT";	(* <INPUT TYPE=TEXT> *)
  ("select"   , "size"   ),  "5";
  ("textarea" , "align"  ),  "bottom";
  ("input"    , "align"  ),  "bottom";
  ("select"   , "align"  ),  "bottom";
  ("img"      , "align"  ),  "bottom";
  (* ("img"   , "alt"    ),  "[IMAGE]"; *) (* Just "IMAGE" ? Boring... *)
  ("area"     , "shape"  ),  "rect";
  ("div"      , "align"  ),  "left";
  ("basefont" , "size"   ),  "3";

  (* frames *)
  ("frame"    , "frameborder" ), "0";
  ("frame"    , "scrolling"   ), "auto";
  ("frameset" , "rows"        ), "100%";
  ("frameset" , "cols"        ), "100%";
  ]
(*e: constant [[Html.default_attributes]] *)

(*s: function [[Html.get_attribute]] *)
let get_attribute tag attr =
  try
    List.assoc attr tag.attributes 
  with Not_found ->
    List.assoc (tag.tag_name, attr) default_attributes
(*e: function [[Html.get_attribute]] *)

(*s: function [[Html.has_attribute]] *)
let has_attribute tag attr =
     List.mem_assoc attr tag.attributes
  || List.mem_assoc (tag.tag_name, attr) default_attributes
(*e: function [[Html.has_attribute]] *)

(*s: type [[Html.length]] *)
(* HTML length *)
type length = 
    Nolength
  | LengthPixels of int
  | LengthRatio of float
  | LengthRel of int
(*e: type [[Html.length]] *)

(*s: function [[Html.length_of_string]] *)
(* Either size in pixels or ration in percent *)
let length_of_string s =
  try
    let pos = String.index s '%' in
    try LengthRatio (float_of_string (String.sub s 0 pos) /. 100.)
    with Failure "int_of_string" -> Nolength
  with Not_found ->
    try
      let pos = String.index s '*' in
      if pos = 0 
      then LengthRel 1
      else
        try LengthRel (int_of_string (String.sub s 0 pos))
        with Failure "int_of_string" -> Nolength
    with Not_found ->
      try LengthPixels (int_of_string s)
      with Failure "int_of_string" -> Nolength
(*e: function [[Html.length_of_string]] *)

(*e: html/html.ml *)

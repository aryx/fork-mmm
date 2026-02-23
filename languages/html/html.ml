(*s: html/html.ml *)
open Printf

(* HTML tokens *)

(*s: type [[Html.attribute_name]] *)
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
 * HTML named character entities, values encoded as UTF-8
 *  cf Appendix B - Proposed Entities
 *)

let ampersand_table =
  (Hashtbl.create 101: (string , string) Hashtbl.t)
(*e: constant [[Html.ampersand_table]] *)

(* Encode a Unicode codepoint as a UTF-8 string *)
let utf8_of_codepoint n =
  if n < 0x80 then
    String.make 1 (Char.chr n)
  else if n < 0x800 then begin
    let b = Bytes.create 2 in
    Bytes.set b 0 (Char.chr (0xC0 lor (n lsr 6)));
    Bytes.set b 1 (Char.chr (0x80 lor (n land 0x3F)));
    Bytes.to_string b
  end else if n < 0x10000 then begin
    let b = Bytes.create 3 in
    Bytes.set b 0 (Char.chr (0xE0 lor (n lsr 12)));
    Bytes.set b 1 (Char.chr (0x80 lor ((n lsr 6) land 0x3F)));
    Bytes.set b 2 (Char.chr (0x80 lor (n land 0x3F)));
    Bytes.to_string b
  end else if n < 0x110000 then begin
    let b = Bytes.create 4 in
    Bytes.set b 0 (Char.chr (0xF0 lor (n lsr 18)));
    Bytes.set b 1 (Char.chr (0x80 lor ((n lsr 12) land 0x3F)));
    Bytes.set b 2 (Char.chr (0x80 lor ((n lsr 6) land 0x3F)));
    Bytes.set b 3 (Char.chr (0x80 lor (n land 0x3F)));
    Bytes.to_string b
  end else " "

(*s: constant [[Html.latin1_normal]] *)
let named_entities = [
  "amp", 	"&";
  "gt", 	">";
  "lt" , 	"<";
  "quot", 	"\"";

  (*s: [[latin1_normal]] elements *)
  (* old: we were generating single-byte latin1 encoding of the special
   * symbols but better to generated instead UTF8 chars as Tk assumes UTF8
   *)
  "nbsp", 	"\194\160"; (* U+00A0 non-breaking space *)
  "iexcl",	"\194\161"; (* U+00A1 inverted exclamation mark *)
  "cent", 	"\194\162"; (* U+00A2 cent sign*)
  "pound",	"\194\163"; (* U+00A3 pound sterling sign*)
  "curren",	"\194\164"; (* U+00A4 general currency sign*)
  "yen",	"\194\165"; (* U+00A5 yen sign*)
  "brvbar",	"\194\166"; (* U+00A6 broken (vertical) bar *)
  "sect",	"\194\167"; (* U+00A7 section sign *)
  "uml",	"\194\168"; (* U+00A8 umlaut (dieresis) *)
  "copy",	"\194\169"; (* U+00A9 copyright sign *)
  "ordf",	"\194\170"; (* U+00AA ordinal indicator, feminine *)
  "laquo",	"\194\171"; (* U+00AB angle quotation mark, left *)
  "not",	"\194\172"; (* U+00AC not sign *)
  "shy",	"\194\173"; (* U+00AD soft hyphen *)
  "reg",	"\194\174"; (* U+00AE registered sign *)
  "macr",	"\194\175"; (* U+00AF macron *)
  "deg",	"\194\176"; (* U+00B0 degree sign *)
  "plusmn",	"\194\177"; (* U+00B1 plus-or-minus sign *)
  "sup2",	"\194\178"; (* U+00B2 superscript two *)
  "sup3",	"\194\179"; (* U+00B3 superscript three *)
  "acute",	"\194\180"; (* U+00B4 acute accent *)
  "micro",	"\194\181"; (* U+00B5 micro sign *)
  "para",	"\194\182"; (* U+00B6 pilcrow (paragraph sign) *)
  "middot",	"\194\183"; (* U+00B7 middle dot *)
  "cedil",	"\194\184"; (* U+00B8 cedilla *)
  "sup1",	"\194\185"; (* U+00B9 superscript one *)
  "ordm",	"\194\186"; (* U+00BA ordinal indicator, masculine *)
  "raquo",	"\194\187"; (* U+00BB angle quotation mark, right *)
  "frac14",	"\194\188"; (* U+00BC fraction one-quarter *)
  "frac12",	"\194\189"; (* U+00BD fraction one-half *)
  "frac34",	"\194\190"; (* U+00BE fraction three-quarters *)
  "iquest",	"\194\191"; (* U+00BF inverted question mark *)
  "Agrave", 	"\195\128"; (* U+00C0 capital A, grave accent *)
  "Aacute", 	"\195\129"; (* U+00C1 capital A, acute accent *)
  "Acirc", 	"\195\130"; (* U+00C2 capital A, circumflex accent *)
  "Atilde", 	"\195\131"; (* U+00C3 capital A, tilde *)
  "Auml", 	"\195\132"; (* U+00C4 capital A, dieresis or umlaut mark *)
  "Aring", 	"\195\133"; (* U+00C5 capital A, ring *)
  "AElig", 	"\195\134"; (* U+00C6 capital AE diphthong (ligature) *)
  "Ccedil", 	"\195\135"; (* U+00C7 capital C, cedilla *)
  "Egrave", 	"\195\136"; (* U+00C8 capital E, grave accent *)
  "Eacute", 	"\195\137"; (* U+00C9 capital E, acute accent *)
  "Ecirc", 	"\195\138"; (* U+00CA capital E, circumflex accent *)
  "Euml", 	"\195\139"; (* U+00CB capital E, dieresis or umlaut mark *)
  "Igrave", 	"\195\140"; (* U+00CC capital I, grave accent *)
  "Iacute", 	"\195\141"; (* U+00CD capital I, acute accent *)
  "Icirc", 	"\195\142"; (* U+00CE capital I, circumflex accent *)
  "Iuml", 	"\195\143"; (* U+00CF capital I, dieresis or umlaut mark *)
  "ETH", 	"\195\144"; (* U+00D0 capital Eth, Icelandic *)
  "Ntilde", 	"\195\145"; (* U+00D1 capital N, tilde *)
  "Ograve", 	"\195\146"; (* U+00D2 capital O, grave accent *)
  "Oacute", 	"\195\147"; (* U+00D3 capital O, acute accent *)
  "Ocirc", 	"\195\148"; (* U+00D4 capital O, circumflex accent *)
  "Otilde", 	"\195\149"; (* U+00D5 capital O, tilde *)
  "Ouml", 	"\195\150"; (* U+00D6 capital O, dieresis or umlaut mark *)
  "times",	"\195\151"; (* U+00D7 multiply sign*)
  "Oslash", 	"\195\152"; (* U+00D8 capital O, slash *)
  "Ugrave", 	"\195\153"; (* U+00D9 capital U, grave accent *)
  "Uacute", 	"\195\154"; (* U+00DA capital U, acute accent *)
  "Ucirc", 	"\195\155"; (* U+00DB capital U, circumflex accent *)
  "Uuml", 	"\195\156"; (* U+00DC capital U, dieresis or umlaut mark *)
  "Yacute",	"\195\157"; (* U+00DD capital Y, acute accent *)
  "THORN", 	"\195\158"; (* U+00DE capital THORN, Icelandic *)
  "szlig", 	"\195\159"; (* U+00DF small sharp s, German (sz ligature) *)
  "agrave", 	"\195\160"; (* U+00E0 small a, grave accent *)
  "aacute", 	"\195\161"; (* U+00E1 small a, acute accent *)
  "acirc", 	"\195\162"; (* U+00E2 small a, circumflex accent *)
  "atilde", 	"\195\163"; (* U+00E3 small a, tilde *)
  "auml", 	"\195\164"; (* U+00E4 small a, dieresis or umlaut mark *)
  "aring", 	"\195\165"; (* U+00E5 small a, ring *)
  "aelig", 	"\195\166"; (* U+00E6 small ae diphthong (ligature) *)
  "ccedil", 	"\195\167"; (* U+00E7 small c, cedilla *)
  "egrave", 	"\195\168"; (* U+00E8 small e, grave accent *)
  "eacute", 	"\195\169"; (* U+00E9 small e, acute accent *)
  "ecirc", 	"\195\170"; (* U+00EA small e, circumflex accent *)
  "euml", 	"\195\171"; (* U+00EB small e, dieresis or umlaut mark *)
  "igrave", 	"\195\172"; (* U+00EC small i, grave accent *)
  "iacute", 	"\195\173"; (* U+00ED small i, acute accent *)
  "icirc", 	"\195\174"; (* U+00EE small i, circumflex accent *)
  "iuml", 	"\195\175"; (* U+00EF small i, dieresis or umlaut mark *)
  "eth", 	"\195\176"; (* U+00F0 small th, Icelandic *)
  "ntilde", 	"\195\177"; (* U+00F1 small n, tilde *)
  "ograve", 	"\195\178"; (* U+00F2 small o, grave accent *)
  "oacute", 	"\195\179"; (* U+00F3 small o, acute accent *)
  "ocirc", 	"\195\180"; (* U+00F4 small o, circumflex accent *)
  "otilde", 	"\195\181"; (* U+00F5 small o, tilde *)
  "ouml", 	"\195\182"; (* U+00F6 small o, dieresis or umlaut mark *)
  "divide",	"\195\183"; (* U+00F7 divide sign *)
  "oslash", 	"\195\184"; (* U+00F8 small o, slash *)
  "ugrave", 	"\195\185"; (* U+00F9 small u, grave accent *)
  "uacute", 	"\195\186"; (* U+00FA small u, acute accent *)
  "ucirc", 	"\195\187"; (* U+00FB small u, circumflex accent *)
  "uuml", 	"\195\188"; (* U+00FC small u, dieresis or umlaut mark *)
  "yacute", 	"\195\189"; (* U+00FD small y, acute accent *)
  "thorn", 	"\195\190"; (* U+00FE small thorn, Icelandic *)
  "yuml", 	"\195\191"  (* U+00FF small y, dieresis or umlaut mark *)
  (*e: [[latin1_normal]] elements *)
]
(*e: constant [[Html.latin1_normal]] *)

(*s: function [[Html.init]] *)
let init _lang =
  named_entities |> List.iter (fun (str, c) ->
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

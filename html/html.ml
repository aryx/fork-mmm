(*s: ./html/html.ml *)
open Printf

(*s: type Html.attribute_name *)
(* HTML tokens *)
type attribute_name = string 
(*e: type Html.attribute_name *)
(*s: type Html.attribute_value *)
type attribute_value = string
(*e: type Html.attribute_value *)
(*s: type Html.attributes *)
type attributes = (attribute_name * attribute_value) list
(*e: type Html.attributes *)

(*s: type Html.tag *)
type tag = {
  tag_name : string;
  attributes: attributes
}
(*e: type Html.tag *)


(*s: type Html.token *)
type token =
   PCData of string
 | CData of string
 | OpenTag of tag
 | CloseTag of string
 | Comment of string
 | Doctype of string
 | EOF
(*e: type Html.token *)

(*s: type Html.location *)
type location = Loc of int * int
(*e: type Html.location *)

(*s: exception Html.Html_Lexing *)
exception Html_Lexing of string * int
(*e: exception Html.Html_Lexing *)
(*s: exception Html.Invalid_Html *)
exception Invalid_Html of string
(*e: exception Html.Invalid_Html *)

(*s: constant Html.verbose *)
let verbose = ref false
(*e: constant Html.verbose *)

(*s: function Html.warning *)
let warning s (Loc(n,m)) = 
  if !verbose then begin 
    eprintf "HTML Warning: %s at (%d, %d)\n" s n m;
    flush stderr
   end
(*e: function Html.warning *)


(*s: function Html.print *)
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
(*e: function Html.print *)

(*s: function Html.beautify *)
(*
 * Remove sequences of white
 *   turns out to be faster than global_replace in libstr
 *   could use String.blit to avoid char copying
 * NOTE: add \0 detection here (we need it for Tk)
 *)
let beautify remove_leading s =
  let j = ref 0
  and white = ref remove_leading in
  for i = 0 to String.length s - 1 do
    match s.[i] with
      ' '|'\t'|'\r'|'\n'|'\000' -> 
    if not !white then begin
       s.[!j] <- ' '; incr j; white := true
       end
    | c -> s.[!j] <- c; white := false; incr j
  done;
  String.sub s 0 !j
(*e: function Html.beautify *)

(*s: function Html.beautify2 *)
(* Remove also trailing space. Used for OPTION tags and TITLE *)
let beautify2 s =
  let s1 = beautify true s in
   match String.length s1 with
     0 | 1 -> s1
   | n -> if s1.[n-1] = ' ' then String.sub s1 0 (n-1) else s1
(*e: function Html.beautify2 *)


(*s: function Html.issp *)
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
(*e: function Html.issp *)
  
(*s: constant Html.ampersand_table *)
(* 
 * HTML encoding of ISO-latin1 characters
 *  cf Appendix B - Proposed Entities
 *)

let ampersand_table = (Hashtbl.create 101: (string , string) Hashtbl.t)
(*e: constant Html.ampersand_table *)

(*s: constant Html.latin1_normal *)
let latin1_normal = [
  "amp", 	"&";
  "gt", 	">";
  "lt" , 	"<";
  "quot", 	"\"";
  "nbsp", 	"\160"; (* non-breaking space *)
  "iexcl",	"\161"; (* ¡ inverted exclamation mark *)
  "cent", 	"\162"; (* ¢ cent sign*)
  "pound",	"\163"; (* £ pound sterling sign*)
  "curren",	"\164"; (* ¤ general currency sign*)
  "yen",	"\165"; (* ¥ yen sign*)
  "brvbar",	"\166"; (* ¦ broken (vertical) bar *)
  "sect",	"\167"; (* § section sign *)
  "uml",	"\168"; (* ¨ umlaut (dieresis) *)
  "copy",	"\169"; (* © copyright sign *)
  "ordf",	"\170"; (* ª ordinal indicator, feminine *)
  "laquo",	"\171"; (* « angle quotation mark, left *)
  "not",	"\172"; (* ¬ not sign *)
  "shy",	"\173"; (* ­ soft hyphen *)
  "reg",	"\174"; (* ® registered sign *)
  "macr",	"\175"; (* ¯ macron *)
  "deg",	"\176"; (* ° degree sign *)
  "plusmn",	"\177"; (* ± plus-or-minus sign *)
  "sup2",	"\178"; (* ² superscript two *)
  "sup3",	"\179"; (* ³ superscript three *)
  "acute",	"\180"; (* ´ acute accent *)
  "micro",	"\181"; (* µ micro sign *)
  "para",	"\182"; (* ¶ pilcrow (paragraph sign) *)
  "middot",	"\183"; (* · middle dot *)
  "cedil",	"\184"; (* ¸ cedilla *)
  "sup1",	"\185"; (* ¹ superscript one *)
  "ordm",	"\186"; (* º ordinal indicator, masculine *)
  "raquo",	"\187"; (* » angle quotation mark, right *)
  "frac14",	"\188"; (* ¼ fraction one-quarter *)
  "frac12",	"\189"; (* ½ fraction one-half *)
  "frac34",	"\190"; (* ¾ fraction three-quarters *)
  "iquest",	"\191"; (* ¿ inverted question mark *)
  "Agrave", 	"\192";	(* À capital A, grave accent *)
  "Aacute", 	"\193";	(* Á capital A, acute accent *)
  "Acirc", 	"\194";	(* Â capital A, circumflex accent *)
  "Atilde", 	"\195";	(* Ã capital A, tilde *)
  "Auml", 	"\196";	(* Ä capital A, dieresis or umlaut mark *)
  "Aring", 	"\197";	(* Å capital A, ring *)
  "AElig", 	"\198";	(* Æ capital AE diphthong (ligature) *)
  "Ccedil", 	"\199";	(* Ç capital C, cedilla *)
  "Egrave", 	"\200";	(* È capital E, grave accent *)
  "Eacute", 	"\201";	(* É capital E, acute accent *)
  "Ecirc", 	"\202";	(* Ê capital E, circumflex accent *)
  "Euml", 	"\203";	(* Ë capital E, dieresis or umlaut mark *)
  "Igrave", 	"\204";	(* Ì capital I, grave accent *)
  "Iacute", 	"\205";	(* Í capital I, acute accent *)
  "Icirc", 	"\206";	(* Î capital I, circumflex accent *)
  "Iuml", 	"\207";	(* Ï capital I, dieresis or umlaut mark *)
  "ETH", 	"\208";	(* Ð capital Eth, Icelandic *)
  "Ntilde", 	"\209";	(* Ñ capital N, tilde *)
  "Ograve", 	"\210";	(* Ò capital O, grave accent *)
  "Oacute", 	"\211";	(* Ó capital O, acute accent *)
  "Ocirc", 	"\212";	(* Ô capital O, circumflex accent *)
  "Otilde", 	"\213";	(* Õ capital O, tilde *)
  "Ouml", 	"\214";	(* Ö capital O, dieresis or umlaut mark *)
  "times",	"\215"; (* × multiply sign*)
  "Oslash", 	"\216";	(* Ø capital O, slash *)
  "Ugrave", 	"\217";	(* Ù capital U, grave accent *)
  "Uacute", 	"\218";	(* Ú capital U, acute accent *)
  "Ucirc", 	"\219";	(* Û capital U, circumflex accent *)
  "Uuml", 	"\220";	(* Ü capital U, dieresis or umlaut mark *)
  "Yacute",	"\221"; (* Ý capital Y, acute accent *)
  "THORN", 	"\222";	(* Þ capital THORN, Icelandic *)
  "szlig", 	"\223";	(* ß small sharp s, German (sz ligature) *)
  "agrave", 	"\224";	(* à small a, grave accent *)
  "aacute", 	"\225";	(* á small a, acute accent *)
  "acirc", 	"\226";	(* â small a, circumflex accent *)
  "atilde", 	"\227";	(* ã small a, tilde *)
  "auml", 	"\228";	(* ä small a, dieresis or umlaut mark *)
  "aring", 	"\229";	(* å small a, ring *)
  "aelig", 	"\230";	(* æ small ae diphthong (ligature) *)
  "ccedil", 	"\231";	(* ç small c, cedilla *)
  "egrave", 	"\232";	(* è small e, grave accent *)
  "eacute", 	"\233";	(* é small e, acute accent *)
  "ecirc", 	"\234";	(* ê small e, circumflex accent *)
  "euml", 	"\235";	(* ë small e, dieresis or umlaut mark *)
  "igrave", 	"\236";	(* ì small i, grave accent *)
  "iacute", 	"\237";	(* í small i, acute accent *)
  "icirc", 	"\238";	(* î small i, circumflex accent *)
  "iuml", 	"\239";	(* ï small i, dieresis or umlaut mark *)
  "eth", 	"\240";	(* ð small eth, Icelandic *)
  "ntilde", 	"\241";	(* ñ small n, tilde *)
  "ograve", 	"\242";	(* ò small o, grave accent *)
  "oacute", 	"\243";	(* ó small o, acute accent *)
  "ocirc", 	"\244";	(* ô small o, circumflex accent *)
  "otilde", 	"\245";	(* õ small o, tilde *)
  "ouml", 	"\246";	(* ö small o, dieresis or umlaut mark *)
  "divide",	"\247"; (* ÷ divide sign *)
  "oslash", 	"\248";	(* ø small o, slash *)
  "ugrave", 	"\249";	(* ù small u, grave accent *)
  "uacute", 	"\250";	(* ú small u, acute accent *)
  "ucirc", 	"\251";	(* û small u, circumflex accent *)
  "uuml", 	"\252";	(* ü small u, dieresis or umlaut mark *)
  "yacute", 	"\253";	(* ý small y, acute accent *)
  "thorn", 	"\254";	(* þ small thorn, Icelandic *)
  "yuml", 	"\255" 	(* ÿ small y, dieresis or umlaut mark *)
  ]
(*e: constant Html.latin1_normal *)

(*s: constant Html.latin1_japan *)
let latin1_japan = [
  "amp", 	"&";
  "gt", 	">";
  "lt" , 	"<";
  "quot", 	"\"";
  "nbsp", 	" "; (* non-breaking space *)
  "iexcl",	"!"; (* ¡ inverted exclamation mark *)
  "cent", 	"ct"; (* ¢ cent sign*)
  "pound",	"Lb"; (* £ pound sterling sign*)
  "curren",	"$"; (* ¤ general currency sign*)
  "yen",	"\\"; (* ¥ yen sign*)
  "brvbar",	"|"; (* ¦ broken (vertical) bar *)
  "sect",	"S"; (* § section sign *)
  "uml",	"\""; (* ¨ umlaut (dieresis) *)
  "copy",	"(c)"; (* © copyright sign *)
  "ordf",	"^a"; (* ª ordinal indicator, feminine *)
  "laquo",	"<<"; (* « angle quotation mark, left *)
  "not",	"\\"; (* ¬ not sign *)
  "shy",	"-"; (* ­ soft hyphen *)
  "reg",	"(r)"; (* ® registered sign *)
  "macr",	"-"; (* ¯ macron *)
  "deg",	"^o"; (* ° degree sign *)
  "plusmn",	"+-"; (* ± plus-or-minus sign *)
  "sup2",	"^2"; (* ² superscript two *)
  "sup3",	"^3"; (* ³ superscript three *)
  "acute",	"'"; (* ´ acute accent *)
  "micro",	"m"; (* µ micro sign *)
  "para",	"$"; (* ¶ pilcrow (paragraph sign) *)
  "middot",	"."; (* · middle dot *)
  "cedil",	","; (* ¸ cedilla *)
  "sup1",	"^1"; (* ¹ superscript one *)
  "ordm",	"^o"; (* º ordinal indicator, masculine *)
  "raquo",	">>"; (* » angle quotation mark, right *)
  "frac14",	"1/4"; (* ¼ fraction one-quarter *)
  "frac12",	"1/2"; (* ½ fraction one-half *)
  "frac34",	"3/4"; (* ¾ fraction three-quarters *)
  "iquest",	"?"; (* ¿ inverted question mark *)
  "Agrave", 	"A`";	(* À capital A, grave accent *)
  "Aacute", 	"A'";	(* Á capital A, acute accent *)
  "Acirc", 	"A";	(* Â capital A, circumflex accent *)
  "Atilde", 	"A~";	(* Ã capital A, tilde *)
  "Auml", 	"A\"";	(* Ä capital A, dieresis or umlaut mark *)
  "Aring", 	"A";	(* Å capital A, ring *)
  "AElig", 	"AE";	(* Æ capital AE diphthong (ligature) *)
  "Ccedil", 	"C,";	(* Ç capital C, cedilla *)
  "Egrave", 	"E`";	(* È capital E, grave accent *)
  "Eacute", 	"E'";	(* É capital E, acute accent *)
  "Ecirc", 	"E";	(* Ê capital E, circumflex accent *)
  "Euml", 	"E\"";	(* Ë capital E, dieresis or umlaut mark *)
  "Igrave", 	"I`";	(* Ì capital I, grave accent *)
  "Iacute", 	"I'";	(* Í capital I, acute accent *)
  "Icirc", 	"I";	(* Î capital I, circumflex accent *)
  "Iuml", 	"I\"";	(* Ï capital I, dieresis or umlaut mark *)
  "ETH", 	"D";	(* Ð capital Eth, Icelandic *)
  "Ntilde", 	"N~";	(* Ñ capital N, tilde *)
  "Ograve", 	"O`";	(* Ò capital O, grave accent *)
  "Oacute", 	"O'";	(* Ó capital O, acute accent *)
  "Ocirc", 	"O";	(* Ô capital O, circumflex accent *)
  "Otilde", 	"O~";	(* Õ capital O, tilde *)
  "Ouml", 	"O\"";	(* Ö capital O, dieresis or umlaut mark *)
  "times",	"x"; (* × multiply sign*)
  "Oslash", 	"O";	(* Ø capital O, slash *)
  "Ugrave", 	"U`";	(* Ù capital U, grave accent *)
  "Uacute", 	"U'";	(* Ú capital U, acute accent *)
  "Ucirc", 	"U";	(* Û capital U, circumflex accent *)
  "Uuml", 	"U\"";	(* Ü capital U, dieresis or umlaut mark *)
  "Yacute",	"Y'"; (* Ý capital Y, acute accent *)
  "THORN", 	"D";	(* Þ capital THORN, Icelandic *)
  "szlig", 	"ss";	(* ß small sharp s, German (sz ligature) *)
  "agrave", 	"a`";	(* à small a, grave accent *)
  "aacute", 	"a'";	(* á small a, acute accent *)
  "acirc", 	"a";	(* â small a, circumflex accent *)
  "atilde", 	"a~";	(* ã small a, tilde *)
  "auml", 	"a\"";	(* ä small a, dieresis or umlaut mark *)
  "aring", 	"a";	(* å small a, ring *)
  "aelig", 	"a";	(* æ small ae diphthong (ligature) *)
  "ccedil", 	"c,";	(* ç small c, cedilla *)
  "egrave", 	"e`";	(* è small e, grave accent *)
  "eacute", 	"e'";	(* é small e, acute accent *)
  "ecirc", 	"e^";	(* ê small e, circumflex accent *)
  "euml", 	"e\"";	(* ë small e, dieresis or umlaut mark *)
  "igrave", 	"i`";	(* ì small i, grave accent *)
  "iacute", 	"i'";	(* í small i, acute accent *)
  "icirc", 	"i^";	(* î small i, circumflex accent *)
  "iuml", 	"i\"";	(* ï small i, dieresis or umlaut mark *)
  "eth", 	"o";	(* ð small eth, Icelandic *)
  "ntilde", 	"n~";	(* ñ small n, tilde *)
  "ograve", 	"o`";	(* ò small o, grave accent *)
  "oacute", 	"o'";	(* ó small o, acute accent *)
  "ocirc", 	"o^";	(* ô small o, circumflex accent *)
  "otilde", 	"o~";	(* õ small o, tilde *)
  "ouml", 	"o\"";	(* ö small o, dieresis or umlaut mark *)
  "divide",	"/"; (* ÷ divide sign *)
  "oslash", 	"o";	(* ø small o, slash *)
  "ugrave", 	"u`";	(* ù small u, grave accent *)
  "uacute", 	"u'";	(* ú small u, acute accent *)
  "ucirc", 	"u^";	(* û small u, circumflex accent *)
  "uuml", 	"u\"";	(* ü small u, dieresis or umlaut mark *)
  "yacute", 	"y'";	(* ý small y, acute accent *)
  "thorn", 	"b";	(* þ small thorn, Icelandic *)
  "yuml", 	"y\"" 	(* ÿ small y, dieresis or umlaut mark *)
  ]
(*e: constant Html.latin1_japan *)

(*s: constant Html.latin1_japan (./html/html.ml) *)
(* To notice Tk that it is not EUC, but ISO8859, we have to put ESC SEQ
   before these entities. If Tk knows the file is JIS already, of course
   this seq is not required at all, but I don't want to write the detection
   code. *)

let latin1_japan = List.map (fun (str,c) ->
  (str, "\027\040\066" ^ c))  latin1_normal
(*e: constant Html.latin1_japan (./html/html.ml) *)

(*s: function Html.init *)
let init japan =
  List.iter (fun (str, c) -> Hashtbl.add ampersand_table str c) 
    (if japan then latin1_japan else latin1_normal) 
(*e: function Html.init *)

(*s: constant Html.get_entity *)
let get_entity = Hashtbl.find ampersand_table
(*e: constant Html.get_entity *)


(*s: constant Html.default_attributes *)
(* Attribute values *)
let default_attributes = [ 
  ("isindex", "prompt"), "Document is indexed/searchable: ";
  ("a", "methods"), "GET";              (* <A METHODS=GET> *)
  ("ol", "type"), "1";			(* <OL TYPE=1 *)
  ("embed", "methods"), "GET";		(* <EMBED METHODS=GET> *)
  ("embed", "alt"), "[EMBEDDED OBJECT]";(* <EMBED ALT="EMBEDDED OBJECT"> *)
  ("form", "method"), "GET";		(* <FORM METHOD=GET> *)
  ("form", "enctype"), "application/x-www-form-urlencoded";
  ("input", "type"), "TEXT";		(* <INPUT TYPE=TEXT> *)
  ("select", "size"), "5";
  ("textarea", "align"), "bottom";
  ("input", "align"), "bottom";
  ("select", "align"), "bottom";
  ("img", "align"), "bottom";
  (* ("img", "alt"), "[IMAGE]"; *) (* Just "IMAGE" ? Boring... *)
  ("area", "shape"), "rect";
  ("div", "align"), "left";
  ("basefont", "size"), "3";
  (* frames *)
  ("frame", "frameborder"), "0";
  ("frame", "scrolling"), "auto";
  ("frameset", "rows"), "100%";
  ("frameset", "cols"), "100%";
  ]
(*e: constant Html.default_attributes *)

(*s: function Html.get_attribute *)
let get_attribute tag attr =
  try
    List.assoc attr tag.attributes 
  with
    Not_found ->
     List.assoc (tag.tag_name, attr) default_attributes
(*e: function Html.get_attribute *)

(*s: function Html.has_attribute *)
let has_attribute tag attr =
     List.mem_assoc attr tag.attributes
  || List.mem_assoc (tag.tag_name, attr) default_attributes
(*e: function Html.has_attribute *)

(*s: type Html.length *)
(* HTML length *)
type length = 
    Nolength
  | LengthPixels of int
  | LengthRatio of float
  | LengthRel of int
(*e: type Html.length *)

(*s: function Html.length_of_string *)
(* Either size in pixels or ration in percent *)
let length_of_string s =
  try
    let pos = String.index s '%' in
    try LengthRatio (float_of_string (String.sub s 0 pos) /. 100.)
    with Failure "int_of_string" -> Nolength
  with
    Not_found ->
      try
       let pos = String.index s '*' in
    if pos = 0 then LengthRel 1
    else
      try LengthRel (int_of_string (String.sub s 0 pos))
      with Failure "int_of_string" -> Nolength
      with
    Not_found ->
      try LengthPixels (int_of_string s)
      with Failure "int_of_string" -> Nolength
(*e: function Html.length_of_string *)

(*e: ./html/html.ml *)

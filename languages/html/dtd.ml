(*s: html/dtd.ml *)
open Printf

(*s: module Dtd.elements *)
module Elements = Set.Make(struct type t = string let compare = compare end)
(*e: module Dtd.elements *)

(*s: type [[Dtd.t]] *)
type t = {
  dtd_name : string;
  contents : (string, Elements.t) Hashtbl.t;
    (* for each element, give the set of included elements *)

  mutable open_omitted : Elements.t;
    (* set of elements for which opening tag may be omitted *)
  mutable close_omitted : Elements.t
    (* set of elements for which closing tag may be omitted *)
 } 
(*e: type [[Dtd.t]] *)

(*s: function [[Dtd.name]] *)
let name t = 
  t.dtd_name
(*e: function [[Dtd.name]] *)

(*s: function [[Dtd.sol]] *)
(* Utils *)
let sol l =
  List.fold_right Elements.add l Elements.empty
(*e: function [[Dtd.sol]] *)
(*s: function [[Dtd.sos]] *)
let sos l =
  List.fold_right Elements.union l Elements.empty
(*e: function [[Dtd.sos]] *)

(*s: constant [[Dtd.dtd20]] *)
(* #PCDATA and #CDATA are considered as elements, but they will never
   be pushed on the stack during evaluation. Moreover, since they are
   not in open_omitted/close_omitted, minimization algorithm will not
   attempt to choose them
 *)

let dtd20 =
  let dtd = {
    dtd_name = "HTML 2.0";

    contents = Hashtbl.create 53;
    open_omitted = Elements.empty;
    close_omitted = Elements.empty
  } in

  let omit_open el =
    dtd.open_omitted <- Elements.add el dtd.open_omitted in
  let omit_close el =
    dtd.close_omitted <- Elements.add el dtd.close_omitted in
  let add_elem = 
    Hashtbl.add dtd.contents in

  (* Some entities *)
  (* <!ENTITY % heading "H1|H2|H3|H4|H5|H6"> *)
  let heading_E = sol ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"]
  (* <!ENTITY % list " UL | OL | DIR | MENU " > *)
  and list_E = sol ["ul"; "ol"; "dir"; "menu"] in
  (* <!ENTITY % font " TT | B | I "> *)
  let font_E = sol ["tt"; "b"; "i"]
  (* <!ENTITY % phrase "EM | STRONG | CODE | SAMP | KBD | VAR | CITE "> *)
  and phrase_E = sol ["em"; "strong"; "code"; "samp"; "kbd"; "var"; "cite"] in
  (* <!ENTITY % text "#PCDATA | A | IMG | BR | %phrase | %font"> *)
  (* EMBED added *)
  let text_E =
    sos [sol ["#pcdata"; "a"; "img"; "br"; "embed"]; font_E; phrase_E] in

  (* <!ELEMENT (%font;|%phrase) - - (%text)*> *)
  Elements.iter (fun e -> add_elem e text_E) font_E;
  Elements.iter (fun e -> add_elem e text_E) phrase_E;

  (* <!ENTITY % pre.content "#PCDATA | A | HR | BR | %font | %phrase"> *)
  let pre_content_E = 
       sos [sol ["#pcdata"; "a"; "hr"; "br"]; font_E; phrase_E] in

  (* <!ELEMENT BR    - O EMPTY> *)
  add_elem "br" Elements.empty;
  omit_close "br";

  (* <!ENTITY % A.content   "(%heading|%text)*"> *)
  let a_content_E = sos [heading_E; text_E] in

  (* <!ELEMENT A     - - %A.content -(A)> *)
  add_elem "a" (Elements.remove "a" a_content_E);

  (* <!ELEMENT IMG    - O EMPTY> *)
  add_elem "img" Elements.empty;
  omit_close "img";

  (* <!ELEMENT P     - O (%text)*> *)
  add_elem "p" text_E;
  omit_close "p";

  (* <!ELEMENT HR    - O EMPTY> *)
  add_elem "hr" Elements.empty;
  omit_close "hr";

  (* <!ELEMENT ( %heading )  - -  (%text;)*> *)
  Elements.iter (fun e -> add_elem e text_E) heading_E;

  (* <!ENTITY % block.forms "BLOCKQUOTE | FORM | ISINDEX"> *)
  let block_forms_E = sol ["blockquote"; "form"; "isindex"] in

  (* <!ENTITY % preformatted "PRE"> *)
  let preformatted_E = sol ["pre"] in

  (* <!ENTITY % block "P | %list | DL
      | %preformatted
      | %block.forms"> *)
  let block_E = sos [sol ["p"; "dl"]; list_E; preformatted_E; block_forms_E] in

  (* <!ENTITY % flow "(%text|%block)*"> *)
  let flow_E = sos [text_E; block_E] in

  (* <!ELEMENT PRE - - (%pre.content)*> *)
  add_elem "pre" pre_content_E;


  (* Deprecated but used <!ELEMENT (XMP|LISTING) - -  %literal> *)
  List.iter (fun e -> add_elem e (sol ["#cdata"])) ["xmp"; "listing"];

  (* <!ELEMENT DL    - -  (DT | DD)+> *)
  add_elem "dl" (sol ["dt"; "dd"]);

  (* <!ELEMENT DT    - O (%text)*> *)
  add_elem "dt" text_E;
  omit_close "dt";

  (* <!ELEMENT DD    - O %flow> *)
  add_elem "dd" flow_E;
  omit_close "dd";

  (* <!ELEMENT (OL|UL) - -  (LI)+> *)
  List.iter (fun e -> add_elem e (sol ["li"])) ["ol"; "ul"];

  (* <!ELEMENT (DIR|MENU) - -  (LI)+ -(%block)> *)
  (* isn't that stupid ? *)
  List.iter (fun e -> add_elem e (sol ["li"])) ["dir"; "menu"];

  (* <!ELEMENT LI    - O %flow> *)
  add_elem "li" flow_E;
  omit_close "li";

  (* <!ENTITY % body.content "(%heading | %text | %block |
                 HR | ADDRESS)*"> *)
  let body_content_E =
     sos [heading_E; text_E; block_E; sol ["hr"; "address"]] in
  
  (* <!ELEMENT BODY O O  %body.content> *)
  add_elem "body" body_content_E;
  omit_open "body";
  omit_close "body";

  (* <!ELEMENT BLOCKQUOTE - - %body.content> *)
  add_elem "blockquote" body_content_E;

  (* <!ELEMENT ADDRESS - - (%text|P)*> *)
  add_elem "address" (Elements.add "p" text_E);

  (* <!ELEMENT FORM - - %body.content -(FORM) +(INPUT|SELECT|TEXTAREA)> *)
  add_elem "form"
    (sos [Elements.remove "form" body_content_E;
      sol ["input";"select";"textarea"]]);

  (* <!ELEMENT INPUT - O EMPTY> *)
  add_elem "input" Elements.empty;
  omit_close "input";

  (* <!ELEMENT SELECT - - (OPTION+) -(INPUT|SELECT|TEXTAREA)> *)
  add_elem "select" (sol ["option"]);

  (* <!ELEMENT OPTION - O (#PCDATA)*> *)
  add_elem "option" (sol ["#pcdata"]);
  omit_close "option";

  (* <!ELEMENT TEXTAREA - - (#PCDATA)* -(INPUT|SELECT|TEXTAREA)> *)
  add_elem "textarea" (sol ["#pcdata"]);

  (* <!ENTITY % head.extra "NEXTID? & META* & LINK*">

     <!ENTITY % head.content "TITLE & ISINDEX? & BASE? &
             (%head.extra)"> *)

  let head_extra_E = sol ["nextid"; "meta"; "link"] in
  let head_content_E = 
    sos [sol ["title"; "isindex"; "base"]; head_extra_E] in
  
  (* <!ELEMENT HEAD O O  (%head.content)> *)
  add_elem "head" head_content_E;
  omit_open "head";
  omit_close "head";

  (* <!ELEMENT TITLE - -  (#PCDATA)*> *)
  add_elem "title" (sol ["#pcdata"]);

  (* <!ELEMENT LINK - O EMPTY> *)
  add_elem "link" Elements.empty;
  omit_close "link";

  (* <!ELEMENT ISINDEX - O EMPTY> *)
  add_elem "isindex" Elements.empty;
  omit_close "isindex";

  (* <!ELEMENT BASE - O EMPTY> *)
  add_elem "base" Elements.empty;
  omit_close "base";

  (* <!ELEMENT NEXTID - O EMPTY> *)
  add_elem "nextid" Elements.empty;
  omit_close "nextid";

  (* <!ELEMENT META - O EMPTY> *)
  add_elem "meta" Elements.empty;
  omit_close "meta";

  (* <!ENTITY % html.content "HEAD, BODY"> *)
  let html_content_E = sol ["head"; "body"] in

  (* <!ELEMENT HTML O O  (%html.content)> *)
  add_elem "html" html_content_E;
  omit_open "html";
  omit_close "html";

  (* fake element PCDATA for minimisation rules *)
  add_elem "#pcdata" Elements.empty;

  (* EMBED is an extension *)
  add_elem "embed" Elements.empty;
  omit_close "embed";
  
  dtd
(*e: constant [[Dtd.dtd20]] *)

(*s: function [[Dtd.dump]] *)
let dump dtd =
  dtd.contents |> Hashtbl.iter (fun s contents -> 
      printf "Element %s %s %s\n" s 
             (if Elements.mem s dtd.open_omitted then "O" else "-")
             (if Elements.mem s dtd.close_omitted then "O" else "-");
      printf "Contains:";
      contents |> Elements.iter (fun e -> printf " %s" e);
      printf "\n"
  )
(*e: function [[Dtd.dump]] *)



(*s: constant [[Dtd.dtd32]] *)
let dtd32 =
  let dtd = {
    dtd_name = "HTML 3.2";
    contents = Hashtbl.create 53;
    open_omitted = Elements.empty;
    close_omitted = Elements.empty
   } in
  let omit_open el =
    dtd.open_omitted <- Elements.add el dtd.open_omitted in
  let omit_close el =
    dtd.close_omitted <- Elements.add el dtd.close_omitted in
  let add_elem = 
    Hashtbl.add dtd.contents in

  let head_misc_E = sol ["script"; "style"; "meta"; "link"]
  and heading_E = sol ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"]
  and list_E = sol ["ul"; "ol"; "dir"; "menu"]
  and preformatted_E = sol ["pre"; "xmp"; "listing"]
  and font_E =
     sol ["tt"; "i"; "b"; "u"; "strike"; "big"; "small"; "sub"; "sup"]
  and phrase_E =
     sol ["em"; "strong"; "dfn"; "code"; "samp"; "kbd"; "var"; "cite"]
  and special_E =
     sol ["a"; "img"; "applet"; "font"; "basefont"; "br"; "script"; "map"]
  and form_E =
     sol ["input"; "select"; "textarea"]
  in
  (* EMBED is not in the original DTD ! *)
  let text_E =
     sos [sol ["#pcdata"; "embed"]; font_E; phrase_E; special_E; form_E]
  in
  Elements.iter (fun e -> add_elem e text_E) font_E;
  Elements.iter (fun e -> add_elem e text_E) phrase_E;
  add_elem "font" text_E;
  add_elem "basefont" Elements.empty;
  omit_close "basefont";
  add_elem "br" Elements.empty;
  omit_close "br";

  let block_E =
    sos [sol ["p"; "dl"; "div"; "center"; "blockquote"; "form"; "isindex";
              "hr"; "table"];
         list_E; preformatted_E]
  in
  let flow_E = sos [text_E; block_E] in
  let body_content_E = sos [sol ["address"]; heading_E; text_E; block_E] in
  add_elem "body" body_content_E;
  omit_open "body";
  omit_close "body";

  let address_content_E = sos [sol ["p"]; text_E] in
  add_elem "address" address_content_E;
  
  add_elem "div" body_content_E;
  add_elem "center" body_content_E;

  add_elem "a" (Elements.remove "a" text_E);
  
  add_elem "map" (sol ["area"]);
  add_elem "area" Elements.empty;
  omit_close "area";

  add_elem "link" Elements.empty;
  omit_close "link";
   
  add_elem "img" Elements.empty;
  omit_close "img";

  add_elem "applet" (Elements.add "param" text_E);
  add_elem "param" Elements.empty;
  omit_close "param";

  add_elem "hr" Elements.empty;
  omit_close "hr";

  add_elem "p" text_E;
  omit_close "p";

  Elements.iter (fun e -> add_elem e text_E) heading_E;

  let pre_exclusion_E = sol ["img"; "big"; "small"; "sub"; "sup"; "font"]
  in
  add_elem "pre" (Elements.diff text_E pre_exclusion_E);

  List.iter (fun e -> add_elem e (sol ["#cdata"])) ["xmp"; "listing"];

  add_elem "blockquote" body_content_E;

  add_elem "dl" (sol ["dt"; "dd"]);
  add_elem "dt" text_E; omit_close "dt";
  add_elem "dd" flow_E; omit_close "dd";

  List.iter (fun e -> add_elem e (sol ["li"])) ["ol"; "ul"];
  List.iter (fun e -> add_elem e (sol ["li"])) ["dir"; "menu"];

  add_elem "li" flow_E;
  omit_close "li";


  add_elem "form" (Elements.remove "form" body_content_E);
  add_elem "input" Elements.empty;
  omit_close "input";
  add_elem "select" (sol ["option"]);
  add_elem "option" (sol ["#pcdata"]);
  omit_close "option";
  add_elem "textarea" (sol ["#pcdata"]);


  add_elem "table" (sol ["caption"; "tr"]);
  add_elem "tr" (sol ["th"; "td"]);
  omit_close "tr";
  List.iter (fun e -> add_elem e body_content_E; omit_close e) ["th"; "td"];
  add_elem "caption" text_E;

  let head_content_E = sol ["title"; "isindex"; "base"] in

  add_elem "head" (Elements.union head_content_E head_misc_E);
  omit_close "head";
  omit_open "head";

  add_elem "title" (sol ["#pcdata"]);
  add_elem "isindex" Elements.empty;
  omit_close "isindex";
  add_elem "base" Elements.empty;
  omit_close "base";
  add_elem "meta" Elements.empty;
  omit_close "meta";

  add_elem "script" (sol ["#cdata"]);
  add_elem "style" (sol ["#cdata"]);

  let html_content_E = sol ["head"; "body"] in

  add_elem "html" html_content_E;
  omit_open "html";
  omit_close "html";

  (* fake element PCDATA for minimisation rules *)
  add_elem "#pcdata" Elements.empty;

  (* embed is an extension *)
  add_elem "embed" Elements.empty;
  omit_close "embed";

  dtd
(*e: constant [[Dtd.dtd32]] *)

(*s: constant [[Dtd.current]] *)
let current = ref dtd32
(*e: constant [[Dtd.current]] *)

(*s: constant [[Dtd.table]] *)
let table = Hashtbl.create 11
(*e: constant [[Dtd.table]] *)

(*s: function [[Dtd.add]] *)
let add t = 
  Hashtbl.add table t.dtd_name t
(*e: function [[Dtd.add]] *)
(*s: constant [[Dtd.get]] *)
let get = 
  Hashtbl.find table
(*e: constant [[Dtd.get]] *)

(*s: function [[Dtd.names]] *)
let names () =
  let names = ref [] in
   Hashtbl.iter (fun name _ -> names := name :: !names) table;
   !names
(*e: function [[Dtd.names]] *)

(*s: toplevel [[Dtd._1]] *)
let _ = add dtd20; add dtd32
(*e: toplevel [[Dtd._1]] *)


(*s: constant [[Dtd.dtd32f]] *)
(* Add frames somwhere to dtd32.
 * Luckily we chose sets, and they are functional
 *)
let dtd32f =
  let dtd = {
    dtd_name = "HTML 3.2 + frames";
    contents = Hashtbl.create 101;
    open_omitted = dtd32.open_omitted;
    close_omitted = dtd32.close_omitted;
  } in
  let _omit_open el =
    dtd.open_omitted <- Elements.add el dtd.open_omitted in
  let omit_close el =
    dtd.close_omitted <- Elements.add el dtd.close_omitted in
  let add_elem = 
   Hashtbl.add dtd.contents in

  (* first : copy in the 3.2 contents *)
  Hashtbl.iter add_elem dtd32.contents;

  (* frameset and frames is pretty simple *)
  add_elem "frameset" (sol ["frameset"; "frame"; "noframes"]);
  add_elem "frame" Elements.empty;
  omit_close "frame";
  (* we say that noframes contains the same thing as body in 3.2 *)
  add_elem "noframes" (Hashtbl.find dtd.contents "body");
  (* and we say that frameset can occur in html *)
  let html_contents = Hashtbl.find dtd.contents "html" in
  Hashtbl.remove dtd.contents "html";
  add_elem "html" (Elements.add "frameset" 
             (Elements.add "noframes" html_contents));
  dtd
(*e: constant [[Dtd.dtd32f]] *)

(*s: toplevel [[Dtd._2]] *)
let _ = add dtd32f
(*e: toplevel [[Dtd._2]] *)
(*e: html/dtd.ml *)

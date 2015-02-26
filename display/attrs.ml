(*s: ./display/attrs.ml *)
open Common

(* Utilities for tags and attributes *)

open Printf
open Protocol
open Tk
open Frx_text
open Fonts

open Htmlfmt

(* Delayed and shared configuration of tags *)

module TagSet = Common.StringSet

class tags (thtml) =
 object
  val mutable onhold = []
  val mutable configured = TagSet.empty
  val mutable decorations = []
  val wid = thtml

  (* define a new tag *)
  method define tagname attrs =
    if TagSet.mem tagname configured then ()
    else begin
      onhold <- (tagname,attrs) :: onhold;
      configured <- TagSet.add tagname configured
    end

  (* change a tag value *)
  method change tagname attrs =
    onhold <- (tagname,attrs) :: onhold;
    configured <- TagSet.add tagname configured

  method add deco =
    decorations <- deco :: decorations

  (* flush tag definitions *)
  method flush =
    onhold |> List.rev |> List.iter (fun (t,d) -> 
      try Text.tag_configure wid t d with TkError _ -> ());
    decorations |> List.rev |> List.iter (fun (t,d,e) -> 
      Text.tag_add wid t d e);
    onhold <- [];
    decorations <- []
end

module LocMap = Ibtree.Make(struct
  type t = index
  let compare = compare
  end)

class anchortags (thtml) =
 object (self)
  inherit tags (thtml) as tags
  inherit Htbind.hypertext (thtml)

  val mutable mappings = []
  val mutable anchor_table = LocMap.empty

  method add_anchor s e h =
    tags#add ("anchor", s, e);
    mappings <- (s,e,h) :: mappings

  method flush =
    tags#flush;
    mappings |> List.iter (fun (s,e,h) ->
      let loc1 = Text.index wid s
      and loc2 = Text.index wid e
      in
      anchor_table <- LocMap.add (loc1,loc2) h anchor_table
    );
    mappings <- []

  method getlink ei =
     (* The index of the click position *)
     let i = 
       Text.index thtml (TextIndex (AtXY (ei.ev_MouseX,ei.ev_MouseY), [])) in
     LocMap.find i anchor_table

  method getrange i = LocMap.find_interval i anchor_table

end


(* Conversion of moral attributes to Tk attributes.
 * This virtual class has to be instantiated for each converter.
 * 'a is an logical attribute description (or "delta")
 *)
class  virtual ['a] nested (tagdef) =
 object (self)
  val mutable last_change = TextIndex(LineChar(0,0),[])
  val mutable stack = []
  (* val tagdef = tagdef *)

  method virtual push_convert : 'a -> string * Tk.options list
  method virtual pop_convert : 'a -> unit

  method put current_pos tagname =
   if last_change <> current_pos then begin
     tagdef#add (tagname, last_change, current_pos);
     last_change <- current_pos
   end

  (* Push some new attribute. *)
  method push current_pos desc =
    let tag, attr = self#push_convert desc in
    tagdef#define tag attr;
    begin match stack with
       [] -> 
        (* no current definition, don't issue a put *)
        last_change <- current_pos
     | curtag::l ->
        self#put current_pos curtag
    end;
    stack <- tag :: stack;

(* Doesn't check the nature of desc *)
  method pop current_pos (desc : 'a) =
     self#pop_convert desc;
     match stack with
       [] ->
        last_change <- current_pos
     | c::l ->
    stack <- l;
        self#put current_pos c
end

(* 
 * Alignment attribute is left/right/center
 *)
class align (tagdef) =
 object
  inherit [string] nested tagdef
  method push_convert ad =
    match String.lowercase ad with
     "right" -> "right", [Justify Justify_Right]
       | "center" -> "center", [Justify Justify_Center]
       | _ -> "left", [Justify Justify_Left]
  method pop_convert ad = ()
end

(*
 * Margin attribute is cumulative
 *)
class margin (tagdef) =
 object
  inherit [int] nested tagdef
  val mutable current = 0
  method push_convert ad =
    current <- current + ad;
    sprintf "margin%d" current, 
    [LMargin1 (Pixels current); LMargin2 (Pixels current)]
  method pop_convert ad =
    current <- current - ad
end


(*
 * Font attributes
 *)

class font (tagdef) =
 object (self)
  inherit [fontInfo list] nested tagdef
  val mutable font_stack = []
  val mutable basefont = !Fonts.default
  method push_convert fil = 
    let curfd = match font_stack with
      [] -> basefont
    | x::l -> x in
    let newfd = Fonts.merge curfd fil in
      font_stack <- newfd :: font_stack;
      Fonts.compute_tag newfd

  method pop_convert _ = 
    match font_stack with
      [] -> ()
    | x::l -> font_stack <- l

  (* by changing the base, we should be changing both the current default size 
     and the behaviour of subsequent FONT SIZE tags. The size changes is easy.
     The header styles being defined with an absolute font, they are not 
     affected
     It's logical also to push this as the current font, but the problem
     is that it doesn't work because basefont do not obey nesting rules
     (consider <FONT> <BASEFONT> </FONT> !). We do deal with this situation.
   *)
  method set_base current_pos n =
    basefont <- { basefont with pxlsz= n };
    self#push current_pos [FontIndex n];

  method pop_all current_pos =
    while font_stack <> [] do
      self#pop current_pos []
    done

end


(*s: constant Attrs.color_mappings *)
(* Special mapping of pre-defined HTML3.2 colors *)
let color_mappings = Hashtbl.create 37
(*e: constant Attrs.color_mappings *)
(*s: toplevel Attrs._1 *)
let _ = List.iter (fun (name, value) -> Hashtbl.add color_mappings name value)
  [ "black",   "#000000";
    "silver",  "#c0c0c0";
    "gray",    "#808080";
    "white",   "#ffffff";
    "maroon",  "#800000";
    "red",     "#ff0000";
    "purple",  "#800080";
    "fuchsia", "#ff00ff";
    "green",   "#008000";
    "lime",    "#00ff00";
    "olive",   "#808000";
    "yellow",  "#ffff00";
    "navy",    "#000080";
    "blue",    "#0000ff";
    "teal",    "#008080";
    "aqua",    "#00ffff" ]
(*e: toplevel Attrs._1 *)

(*s: function Attrs.html_color *)
let html_color s =
  try Hashtbl.find color_mappings (String.lowercase s)
  with Not_found -> s
(*e: function Attrs.html_color *)

(*
 * Foreground color
 *)

class fgcolor (tagdef) =
 object
  inherit [string] nested tagdef 
  method push_convert s =
    let colordef = html_color s in
    if Frx_color.check colordef then
      s, [Foreground (NamedColor colordef)]
    else
      s, []
  method pop_convert s = 
    ()
end

(*
 * Background color
 *)

class bgcolor (tagdef) =
 object
  inherit [string] nested tagdef 
  method push_convert s =
    let colordef = html_color s in
    if Frx_color.check colordef then
      s, [Background (NamedColor colordef)]
    else 
      s, []
  method pop_convert s = 
    ()
end

(*
 * Super and sub script.
 * BOGUS: should depend on current font size
 *)
class offset (tagdef) =
 object
  inherit [int] nested tagdef
  val mutable cur_offset = 0
  method push_convert n =
    cur_offset <- cur_offset + n;
    sprintf "offset%d" cur_offset, [Offset (Pixels cur_offset)]
  method pop_convert n =
    cur_offset <- cur_offset - n
end
(* 
 * Other stuff where nesting is not important
*)
class misc (tagdef, tagname, attr) =
 object (self)
  
  val mutable start_pos = TextIndex(LineChar(0,0),[])
  (* val tagdef = tagdef *)
  val tagname = let _ = tagdef#define tagname  attr in
     tagname

  method pop current_pos =
   if start_pos <> current_pos then begin
     tagdef#add (tagname, start_pos, current_pos)
   end

  method push current_pos  =
     start_pos <- current_pos

end

(* 
 * Spacing is specific, due to Tk's line model and BR
 *  push corresponds to top spacing for the first line
 *  pop corresponds to bottom spacing for the first line
 *)

class spacing (tagdef) =
 object
  (* val tagdef = tagdef *)
 
  method push current_pos n =
    let topname = sprintf "topspace%d" n in
     tagdef#define topname [Spacing1 (Pixels n)];
     match current_pos with
       TextIndex(base,[x]) ->
     tagdef#add (topname, TextIndex(base, [x;LineStart]),
                      TextIndex(base, [x;LineEnd]));
     ()
     | _ -> assert false

  method pop current_pos n =
    let botname = sprintf "botspace%d" n in
    tagdef#define botname [Spacing3 (Pixels n)];
     match current_pos with
       TextIndex(base,[x]) ->
     tagdef#add (botname, TextIndex(base, [x;LineStart]),
                      TextIndex(base, [x;LineEnd]));
     ()
     | _ -> assert false
end

(*s: constant Attrs.circle_data *)
(* Bullet images *)
let circle_data = 
"#define circle_width 9
#define circle_height 9
static unsigned char circle_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x38, 0x00, 0x44, 0x00, 0x44, 0x00, 0x44, 0x00,
   0x38, 0x00, 0x00, 0x00, 0x00, 0x00};"
(*e: constant Attrs.circle_data *)

(*s: constant Attrs.disc_data *)
let disc_data =
"#define disc_width 9
#define disc_height 9
static unsigned char disc_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x38, 0x00, 0x7c, 0x00, 0x7c, 0x00, 0x7c, 0x00,
   0x38, 0x00, 0x00, 0x00, 0x00, 0x00};"
(*e: constant Attrs.disc_data *)

(*s: constant Attrs.square_data *)
let square_data = 
"#define square_width 9
#define square_height 9
static unsigned char square_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x7c, 0x00, 0x7c, 0x00, 0x7c, 0x00, 0x7c, 0x00,
   0x7c, 0x00, 0x00, 0x00, 0x00, 0x00};"
(*e: constant Attrs.square_data *)

(*s: constant Attrs.bullet_table *)
let bullet_table = Hashtbl.create 11
(*e: constant Attrs.bullet_table *)
(*s: function Attrs.init *)
let init bg =
  let _bgTODO = Background (NamedColor bg) in
  Hashtbl.add bullet_table
     "circle" (ImageBitmap(Imagebitmap.create [Data circle_data]));
  Hashtbl.add bullet_table
     "disc" (ImageBitmap(Imagebitmap.create [Data disc_data]));
  Hashtbl.add bullet_table
     "square" (ImageBitmap(Imagebitmap.create [Data square_data]))
(*e: function Attrs.init *)
(*e: ./display/attrs.ml *)

(*s: ./display/html_disp.ml *)
(* HTML Display Machine *)
open Printf
open Html
open Htmlfmt
open Hyper
open Www
open Document
open Maps
open Embed
open Viewers
open Fonts

(*s: constant Html_disp.verbose *)
let verbose = ref false
(*e: constant Html_disp.verbose *)
(*s: constant Html_disp.attempt_tables *)
(* Preference settings *)
let attempt_tables = ref false
(*e: constant Html_disp.attempt_tables *)

(*s: function Html_disp.lowernumber *)
(* SMOP Utilities for OL numbering *)
let lowernumber n =
  let rec f cur n = 
    if n < 0 then cur
    else  f (String.make 1 (Char.chr (97 + n mod 26)) ^ cur) (n / 26 - 1)
  in 
  if n <= 0 then "*" else f "" (n-1)
(*e: function Html_disp.lowernumber *)

(*s: function Html_disp.uppernumber *)
let uppernumber n =
  let rec f cur n = 
    if n < 0 then cur
    else  f (String.make 1 (Char.chr (64 + n mod 26)) ^ cur) (n / 26 - 1)
  in
  if n <= 0 then "*" else f "" (n-1)
(*e: function Html_disp.uppernumber *)

(*s: constant Html_disp.romans *)
let romans = [|
  [| ""; "I"; "II"; "III"; "IV"; "V"; "VI"; "VII"; "VIII"; "IX" |];
  [| ""; "X"; "XX"; "XXX"; "XL"; "L"; "LX"; "LXX"; "LXXX"; "XC" |];
  [| ""; "C"; "CC"; "CCC"; "CD"; "D"; "DC"; "DCC"; "DCCC"; "CM" |];
  [| ""; "M"; "MM"; "MMM"; "*MMM"; "*MMM"; "*MMM"; "*MMM"; "*MMM"; "*MMM" |];
  |]
(*e: constant Html_disp.romans *)

(*s: function Html_disp.roman *)
let roman n =
  let rec r cur level n =
    if n = 0 then cur
    else if level > 3 then "*" ^ cur
    else r (romans.(level).(n mod 10) ^ cur) (succ level)  (n / 10)
  in if n <= 0 then "*" else r "" 0 n
(*e: function Html_disp.roman *)

(*s: class Html_disp.imgloader *)
class  virtual imgloader (unit : unit) =
 object
  (*s: [[Html_disp.imgloader]] virtual fields signatures *)
  method virtual add_image : Embed.embobject -> unit	 (* add one image *)
  method virtual flush_images : unit	         (* flush when document is loaded *)
  method virtual load_images : unit		 (* manual flush *)
  method virtual update_images : unit
  (*e: [[Html_disp.imgloader]] virtual fields signatures *)
end
(*e: class Html_disp.imgloader *)

(*s: class Html_disp.machine *)
class  virtual machine (unit : unit) =
 object
   (*s: [[Html_disp.machine]] virtual fields signatures *)
   method virtual ctx : Viewers.context

   method virtual base : string
   method virtual set_base : string -> unit
   method virtual target : string option
   method virtual set_target : string -> unit

   method virtual formatter : Htmlfmt.formatter
   method virtual imgmanager : imgloader

   method virtual add_tag: 
     string -> (Htmlfmt.formatter -> Html.tag -> unit) -> 
     (Htmlfmt.formatter -> unit) -> 
     unit
   method virtual get_tag : 
     string -> 
     (Htmlfmt.formatter -> Html.tag -> unit) * (Htmlfmt.formatter -> unit)
   method virtual remove_tag : string -> unit

   method virtual push_action : (string -> unit) -> unit
   method virtual pop_action : unit

   method virtual push_formatter : Htmlfmt.formatter -> unit
   method virtual pop_formatter : Htmlfmt.formatter

   method virtual create_formatter : 
       Htmlfmt.formatterSpec -> Widget.widget -> 
     Htmlfmt.formatter * Widget.widget
   method virtual send : Html.token -> unit
   method virtual look_for : Html.token -> unit

   method virtual add_embedded : Embed.embobject -> unit
   method virtual embedded : Embed.embobject list

   method virtual see_frag : string option -> unit
  
   (* For other languages) *)
   (* encode the internal i18n strings to corresponding encodings *)
   method virtual i18n_encoder : string -> string
   method virtual set_i18n_encoder : (string -> string) -> unit
   (*e: [[Html_disp.machine]] virtual fields signatures *)
end
(*e: class Html_disp.machine *)

(*s: constant Html_disp.user_hooks *)
(* Hooks for applets/modules. Control is made elsewhere *)
let user_hooks = ref []
(*e: constant Html_disp.user_hooks *)
(*s: function Html_disp.add_hook *)
let add_hook f = 
  user_hooks := f :: !user_hooks
(*e: function Html_disp.add_hook *)

(*s: constant Html_disp.default_fo *)
(* This is the default formatter *)
let default_fo = {
  new_paragraph = (fun () -> ());
  close_paragraph = (fun () -> ());
  print_newline = (fun b -> ());
  print_verbatim = (fun s -> ());
  format_string = (fun s -> ());
  hr = (fun l n b -> ());
  bullet = (fun n -> ());
  set_defaults = (fun s l -> ());
  push_attr = (fun l -> ());
  pop_attr = (fun l -> ());
  isindex = (fun s s' -> ());
  start_anchor = (fun () -> ());
  end_anchor = (fun h -> ());
  add_mark = (fun _ -> ());
  create_embedded = (fun a w h -> assert false);
  see_frag = (fun _ -> ());
  flush = (fun () -> ());
} 
(*e: constant Html_disp.default_fo *)

(*s: function Html_disp.push_style *)
(* Style abbreviation 
 * TODO?: check stack.
 *)
let push_style fo s =
  try fo.push_attr (Styles.get s)
  with Not_found -> Log.f (sprintf "Missing style : %s" s)
(*e: function Html_disp.push_style *)

(*s: function Html_disp.pop_style *)
let pop_style fo s =
  try fo.pop_attr (Styles.get s)
  with Not_found -> Log.f (sprintf "Missing style : %s" s)
(*e: function Html_disp.pop_style *)

(*s: functor Html_disp.Make *)
module Make (G : GfxHTML) (F: FormDisplay) (T: TableDisplay) = struct
  module FormLogic = Html_form.Make(F)
  module TableLogic = Html_table.Make(T)

  (*s: type Html_disp.Make.anchor_type *)
  type anchor_type = HREF | NAME
  (*e: type Html_disp.Make.anchor_type *)

  (* Tag machinery *)
  (*s: type Html_disp.Make.html_behavior *)
  type html_behavior = {
    tag_open  : formatter -> tag -> unit;
    tag_close : formatter -> unit
  }
  (*e: type Html_disp.Make.html_behavior *)
    
  (*s: functions Html_disp.Make.ignore_xxx *)
  let ignore_open = fun _ _ -> ()
  let ignore_close = fun _ -> ()
  (*e: functions Html_disp.Make.ignore_xxx *)

  (*s: class Html_disp.Make.display_machine *)
  class display_machine (ctx, imgmanager) =
   object (self) 
      inherit machine ()

      (* Keep a copy of the arguments *) 
      (* val ctx = (ctx : Viewers.context)  JPF: for ocaml2.00 *)
      (* val imgmanager = imgmanager *)
      method ctx = ctx
      method imgmanager = imgmanager    

      (* record all embedded objects in this machine *)
      val mutable (*private*) embedded = []
      method add_embedded x = 
        Embed.add x;
        embedded <- x :: embedded
      method embedded = embedded

      val mutable base = Url.string_of ctx#base.document_url
      method base = base
      method set_base s = base <- s

      val mutable target = None
      method target = target
      method set_target t = target <- Some t

      val (*private*) tags = (Hashtbl.create 101 : (string, html_behavior) Hashtbl.t)
      val mutable (*private*) action = (fun s -> ())
      val mutable (*private*) action_stack = []
      val mutable (*private*) formatter_stack = []
    
      (* Accessing the variables *)
      val mutable (*private*) formatter = default_fo
      method formatter = formatter

      (* Adding and removing tag behaviors *)
      method add_tag t o c = 
        Hashtbl.add tags t {tag_open = o; tag_close = c}
      method get_tag t =
        let {tag_open = o; tag_close = c} = Hashtbl.find tags t in 
        o,c
      method remove_tag = Hashtbl.remove tags

      (* Changing the default mode for pcdata and cdata *)
      method push_action f =
        action_stack <-  f :: action_stack;
        action <- f
      method pop_action =
        match action_stack with
        | [] -> Log.f "Warning: empty action stack"
        |	old::l ->
        action_stack <- l;
        action <- match l with [] -> (fun s ->()) | newa::_ -> newa

      (* This is an intrusion of graphics, but I don't see any other way 
       * The last formatter always tries see_frag...
       *)
      val mutable see_frag = (fun _ -> ())
      method see_frag = see_frag

      (* Nested formatters for table cells and other usage *)
      method push_formatter fo =
        formatter <- fo;
        formatter_stack <- fo :: formatter_stack;
        self#push_action fo.format_string;
        see_frag <- fo.see_frag

      method pop_formatter =
        self#pop_action;
        match formatter_stack with
      [] -> 
        Log.f "Warning: empty formatter stack";
        default_fo
        | old::l ->
        old.flush();
        see_frag <- old.see_frag;
        formatter_stack <- l;
        formatter <- (match l with [] -> default_fo | newf :: _ -> newf);
        old

      (* This is only for robustness *)
      method flush_formatters =
        while List.length formatter_stack > 0 do
      Log.f "WARNING: too many formatters in stack";
      self#pop_formatter.flush()
        done


      (* Nested windows *)
      val table_namer = Mstring.egensym "tablecell"
      method create_formatter spec w = G.create table_namer spec w ctx

      (* ignore everything up to some tag *)
      val mutable look_for = None

      method look_for e = 
        look_for <- Some e

      (* Dispatching a token *)
      method private normal_send = function
      EOF -> self#flush_formatters;
        | CData s -> action s
        | PCData s -> action s
        | OpenTag t ->
         begin try
           let tag = Hashtbl.find tags t.tag_name in
             tag.tag_open formatter t
         with
           Not_found ->
             if !verbose then
           Log.f (sprintf "Display machine: <%s> ignored" t.tag_name)
         end
        | CloseTag n ->
         begin try
           (Hashtbl.find tags n).tag_close formatter
         with
           Not_found ->
             if !verbose then
           Log.f (sprintf "Display machine: </%s> ignored" n)
         end
        | Comment _ -> ()
        | Doctype _ -> ()

     method send tok =
       match look_for with
         None -> self#normal_send tok
       | Some it when it = tok -> 
       self#normal_send tok;
       look_for <- None
       | _ -> ()
     
     val mutable i18n_encoder = (fun s -> s : string -> string)
     method i18n_encoder = i18n_encoder
     method set_i18n_encoder enc = i18n_encoder <- enc
  end
  (*e: class Html_disp.Make.display_machine *)

  (*s: function Html_disp.Make.init *)
  (* Standard initialisation for HTML 2.0 (+bits of 3.2) *)
  let init mach =
    (*s: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (* 5.1 HTML *)
    mach#add_tag "html" ignore_open ignore_close;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.2 Head: <HEAD>
     * <!ENTITY % head.content "TITLE & ISINDEX? & BASE? %head.extra">
     * <!ENTITY % head.extra "& NEXTID?">
     * NOTE: this is now handled elsewhere
     *)
    mach#add_tag "head" ignore_open ignore_close;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.2.1 Title: <TITLE>
     * assumes a unique Text token inside since
     * <!ELEMENT TITLE - -  (#PCDATA)*>
     * the title is not printed
     * NOTE: this is now handled elsewhere
     *)
    mach#add_tag "title" ignore_open ignore_close;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.2.2 Base Address: <BASE>
     * TARGET is from PR-HTML4.0
     *)
    mach#add_tag "base"
        (fun fo tag ->
          begin 
            try mach#set_target (get_attribute tag "target")
            with Not_found -> ()
          end;
          begin 
            try mach#set_base (get_attribute tag "href")
            with Not_found -> raise (Invalid_Html "HREF required in BASE")
          end)
        ignore_close
    ;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.2.3 Keyword Index: <ISINDEX>
     * HTML3.2: PROMPT attribute (default given)
     * NOTE: ISINDEX in HEAD is handled elsewhere, but we must keep it
     *       here because it may appear in BODY
     *)
    mach#add_tag "isindex"
       (fun fo t -> fo.isindex (get_attribute t "prompt") mach#base)
       ignore_close
    ;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.2.4 Link: <LINK>
     * 5.2.5 Associated Meta-information: <META>
     * 5.2.6 Next Id: <NEXTID>
     * NOTE: this is now handled elsewhere (only in HEAD)
     *)
    ["link"; "meta"; "nextid"] |> List.iter (fun t -> 
      mach#add_tag t ignore_open ignore_close
    );
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.3 Body: <BODY>
     * <!ENTITY % html.content "HEAD, BODY">
     * Note: with textw_fo, flush disables the text widget, so anything
       beyond </BODY> will not be displayed. Some documents have multiple
       bodies, or </BODY> before the end of the document. So we decide
       to completely ignore this tag. A stricter interpretation would be
       {tag_open = ...; tag_close = (fun fo -> fo.flush())};
       Our simpled minded minimization rules also introduce multiple BODY.
     *)
    mach#add_tag "body" ignore_open ignore_close;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.4 Headings <H1> ... <H6>
     * <!ELEMENT ( %heading )  - -  (%text;)*>
     * Assume headings may contain typographic styles, anchors
     * HTML3.2
     * <!ATTLIST ( %heading )
     *         align  (left|center|right) #IMPLIED
     *         >
     *)

    (*s: [[Html_disp.Make.init()]] headings private variables *)
    (* Private variables of header *)
    let header_size = ref 0 in
    let header_align = ref None in
    (*e: [[Html_disp.Make.init()]] headings private variables *)
    (*s: function Html_disp.Make.init.open_header *)
    let open_header size = 
     fun fo tag ->
      fo.new_paragraph() ;
      header_size := size;
      push_style fo (sprintf "header%d" size);
      try
        let align = Html.get_attribute tag "align" in 
        fo.push_attr [Justification align];
        header_align := Some align
      with Not_found -> header_align := None
    in
    (*e: function Html_disp.Make.init.open_header *)
    (*s: function Html_disp.Make.init.close_header *)
    let close_header fo =
      pop_style fo (sprintf "header%d" !header_size);
      fo.close_paragraph();
      match !header_align with
      | None -> ()
      | Some a -> fo.pop_attr [Justification a]
    in
    (*e: function Html_disp.Make.init.close_header *)
    [1;2;3;4;5;6] |> List.iter (fun headnum ->
      mach#add_tag (sprintf "h%d" headnum) (open_header headnum) close_header
    );
         
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.5.1 Paragraph: <P>
     *   a bit approximative in HTML 2.0
     * HTML3.2
     * <!ATTLIST P
     *         align  (left|center|right) #IMPLIED
     *         >
     *)
    let paligns = ref [] in

    mach#add_tag "p" 
      (fun fo tag -> 
         fo.new_paragraph ();
         try
           let a = get_attribute tag "align" in
           paligns := (Some a) :: !paligns;
           fo.push_attr [Justification a]
         with Not_found -> paligns := None :: !paligns)
      (fun fo ->
         fo.close_paragraph();
         match !paligns with
         | [] -> () (* that's an error actually *)
         | (Some a)::l ->
             fo.pop_attr [Justification a];
             paligns := l
         | None::l ->
             paligns := l)
    ;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (* 
     * 5.5.2 Preformatted Text : <PRE>
     *    TODO: optional attribute WIDTH
     *    should be fixed font, respecting newlines
     *    local styles authorized however (i.e. markup is parsed)
    *)
    (* 5.5.2.1 Example and Listing: <XMP>, <LISTING>
     *    deprecated anyway
     *)

    ["pre"; "listing"; "xmp"] |> List.iter (fun s -> 
      mach#add_tag s 
        (fun fo tag ->
           fo.new_paragraph();
           push_style fo "verbatim";
           mach#push_action fo.print_verbatim)
        (fun fo ->
           pop_style fo "verbatim";
           fo.close_paragraph();
           mach#pop_action)
    );
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.5.3 Address: <ADDRESS>
     *)
    mach#add_tag "address" 
       (fun fo tag -> fo.new_paragraph(); push_style fo "italic")
       (fun fo -> pop_style fo "italic"; fo.close_paragraph())
    ;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.5.4 Block Quote: <BLOCKQUOTE>
     *)
    mach#add_tag "blockquote"
       (fun fo tag ->
          fo.new_paragraph();
          push_style fo "italic";
          fo.push_attr [Margin 10])
       (fun fo ->
          pop_style fo "italic";
          fo.pop_attr [Margin 10];
          fo.close_paragraph())
    ;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.7.1.1 Citation: <CITE>
     * 5.7.1.2 Code: <CODE>
     * 5.7.1.3 Emphasis: <EM>
     * 5.7.1.4 Keyboard: <KBD>
     * 5.7.1.5 Sample: <SAMP>
     * 5.7.1.6 Strong Emphasis: <STRONG>
     * 5.7.1.7 Variable: <VAR>
     *)

    (* Different typographic styles, shared *)
    let italic_style t = 
      mach#add_tag t 
         (fun fo tag -> push_style fo "italic")
         (fun fo -> pop_style fo "italic")
    in
    let fixed_style t =
      mach#add_tag t 
        (fun fo tag -> push_style fo "fixed")
        (fun fo -> pop_style fo "fixed")
    in
    let bold_style t =
      mach#add_tag t 
        (fun fo tag -> push_style fo "bold")
        (fun fo -> pop_style fo "bold")
    in
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.7.2.1 Bold: <B>
     * 5.7.2.2 Italic: <I>
     * 5.7.2.3 Teletype: <TT>
     *)
    bold_style "b";
    italic_style "i";
    fixed_style "tt";
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    ["cite"; "em"; "var"] |> List.iter italic_style;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    ["code"; "kbd"; "samp"] |> List.iter fixed_style;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    bold_style "strong";
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (* Some HTML 3.2 flashy features *)
    mach#add_tag "u"
      (fun fo t -> fo.push_attr [Underlined])
      (fun fo  -> fo.pop_attr [Underlined]);
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    mach#add_tag "strike"
      (fun fo t -> fo.push_attr [Striked])
      (fun fo  -> fo.pop_attr [Striked]);
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    mach#add_tag "sup"
      (fun fo t -> fo.push_attr [Superscript])
      (fun fo  -> fo.pop_attr [Superscript]);
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    mach#add_tag "sub"
      (fun fo t -> fo.push_attr [Lowerscript])
      (fun fo  -> fo.pop_attr [Lowerscript]);
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (* Some HTML 3.2 flashy features *)
    mach#add_tag "center"
        (fun fo t -> fo.push_attr [Justification "center"])
        (fun fo -> fo.pop_attr [Justification "center"]);
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (* Some HTML 3.2 flashy features *)
      mach#add_tag "div"
          (fun fo t -> fo.push_attr [Justification (get_attribute t "align")])
          (fun fo -> fo.pop_attr [Justification "whocares"]);
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (* Some HTML 3.2 flashy features *)
      mach#add_tag "big"
          (fun fo t -> fo.push_attr [Font (FontDelta 2)])
          (fun fo  -> fo.pop_attr [Font (FontDelta 2)]);
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (* Some HTML 3.2 flashy features *)
      mach#add_tag "small"
          (fun fo t -> fo.push_attr [Font (FontDelta (-2))])
          (fun fo  -> fo.pop_attr [Font (FontDelta (-2))]);
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.6.1 Unordered List: <UL>, <LI>
     * HTML3.2 
     * <!ENTITY % ULStyle "disc|square|circle">
     * 
     * <!ATTLIST UL -- unordered lists --
     *         type    (%ULStyle)   #IMPLIED   -- bullet style --
     *         compact (compact)    #IMPLIED   -- reduced interitem spacing --
     *         >
     *)
    let list_level = ref 0 in

    let open_list fo tag =
      fo.push_attr [Margin 10];
      incr list_level;
      let bullet = 
        try get_attribute tag "type" 
        with Not_found ->
           (match !list_level mod 3 with
           | 1 -> "disc" 
           | 2 -> "circle" 
           | _ -> "square" 
           )
      in
      let compact = has_attribute tag "compact" in
      let first_line = ref true in
      fo.new_paragraph();
      mach#add_tag "li"
         (fun fo tag -> 
            if !first_line 
            then first_line := false
            else 
              if compact 
              then fo.print_newline false 
              else fo.new_paragraph();
            let bullet = try get_attribute tag "type" with Not_found -> bullet in
         fo.bullet bullet)
         (fun fo -> if not compact then fo.close_paragraph())
    in

    let close_list fo =
      decr list_level;
      fo.close_paragraph();
      fo.pop_attr [Margin 10];
      mach#remove_tag "li"
    in

    mach#add_tag "ul" open_list close_list;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (* 
     * 5.6.2 Ordered List: <OL>, <LI>
     * HTML3.2
     * <!--
     *        Numbering style
     *     1   arablic numbers     1, 2, 3, ...
     *     a   lower alpha         a, b, c, ...
     *     A   upper alpha         A, B, C, ...
     *     i   lower roman         i, ii, iii, ...
     *     I   upper roman         I, II, III, ...
     * 
     *     The style is applied to the sequence number which by default
     *     is reset to 1 for the first list item in an ordered list.
     * -->
     * 
     * <!ENTITY % OLStyle "CDATA" -- "1|a|A|i|I" but SGML folds case -->
     * 
     * <!ATTLIST OL -- ordered lists --
     *         type     (%OLStyle)  #IMPLIED   -- numbering style --
     *         start     NUMBER     #IMPLIED   -- starting sequence number --
     *         compact  (compact)   #IMPLIED   -- reduced interitem spacing --
     *         >
     *)

    let numbering_styles =
      ["1", string_of_int;
       "a", lowernumber;
       "A", uppernumber;
       "i", (function i -> String.lowercase (roman i));
       "I", roman
      ]
    in

    let nesting = ref [] in

    let open_nlist fo tag =
      let li_counter = 
        ref (try int_of_string (get_attribute tag "start")
             with _ -> 1) 
      in
      fo.push_attr [Margin 10];
      nesting := li_counter :: !nesting;
      let thisnumbers = List.rev !nesting in
      let numbering = 
         try List.assoc (get_attribute tag "type") numbering_styles 
         with Not_found -> string_of_int
      in
      let compact = has_attribute tag "compact" in
      mach#add_tag "li"
        (fun fo tag ->
          fo.new_paragraph();
          if compact 
          then fo.push_attr [Spacing 0];
          (* if value is given, use it as number *)
          begin
            try
              let n = int_of_string (get_attribute tag "value") in
              match !nesting with
              | c::_ -> c := n
              |	_ -> () (* assert false *)
            with Not_found | Failure "int_of_string" -> ()
          end;
          thisnumbers |> List.iter (fun i ->
            fo.format_string (numbering !i);
            fo.format_string "."
          ))
        (fun fo ->
          incr li_counter;
          if compact 
          then fo.pop_attr [Spacing 0];
          fo.close_paragraph())
    in

    let close_nlist fo =
      fo.pop_attr [Margin 10];
      nesting := 
        (match !nesting with
        | [] -> [] 
        | x::l -> l 
        );
      mach#remove_tag "li"
    in

    mach#add_tag "ol" open_nlist close_nlist;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.6.3 Directory List: <DIR>
     * 5.6.4 Menu List: <MENU>
     *  do as <UL>, but we should work on presentation
     *)
    mach#add_tag "dir" open_list close_list;
    mach#add_tag "menu" open_list close_list;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.6.5 Definition List: <DL>, <DT>, <DD> 
     *)
    let open_dl fo tag =
      let compact = Html.has_attribute tag "compact" in
      fo.new_paragraph();
      fo.push_attr [Margin 10];

      if not compact then begin
        let prev_is_dt = ref false in

        mach#add_tag "dt"
          (fun fo tag -> 
             if not !prev_is_dt then begin
               fo.new_paragraph();
               prev_is_dt := true
             end else 
               fo.print_newline false;
             push_style fo "bold")
          (fun fo -> pop_style fo "bold");

        mach#add_tag "dd"
          (fun fo tag ->
             if !prev_is_dt then begin
               fo.close_paragraph();
               prev_is_dt := false
             end;
             fo.new_paragraph();
             fo.push_attr [Margin 20])
          (fun fo ->
              fo.pop_attr [Margin 20];
              fo.close_paragraph())

        end else begin
          (* if compact *)
          let first_item = ref true in

          mach#add_tag "dt"
            (fun fo tag -> 
               if not !first_item 
               then fo.print_newline false
               else first_item := false;
               push_style fo "bold")
            (fun fo -> pop_style fo "bold");

          mach#add_tag "dd"
            (fun fo tag ->
               if not !first_item 
               then fo.print_newline false
               else first_item := false;
               fo.push_attr [Margin 20])
            (fun fo -> fo.pop_attr [Margin 20])
        end
    in

    let close_dl fo =
      fo.pop_attr [Margin 10];
      fo.close_paragraph();
      mach#remove_tag "dt";
      mach#remove_tag "dd";
    in

    mach#add_tag "dl" open_dl close_dl;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.7.3 Anchor: <A> 
     * Assumes anchors are not nested
     * Can be both HREF and NAME.
     *)

    let anchor_type = ref None in
    let anchor_link = ref (Hyper.default_link "") in
    let in_anchor = ref false in

    let open_anchor fo tag =
      anchor_type := None;
      (*s: [[Html_disp.Make.init.open_anchor()]] look for NAME attribute *)
      (* is there a NAME attribute ? *)
      begin
        try 
          fo.add_mark (Html.get_attribute tag "name");
          anchor_type := Some NAME
        with Not_found -> ()
      end;
      (*e: [[Html_disp.Make.init.open_anchor()]] look for NAME attribute *)
      (*s: [[Html_disp.Make.init.open_anchor()]] look for HREF attribute *)
      (* is there an HREF attribute ? (if both, anchor_type is set to HREF *)
      (* so that close_anchor does the right thing) *)
      begin
        try
          let href = Html.get_attribute tag "href" in
          let h_params =
            try ["target", Html.get_attribute tag "target"]
            with Not_found ->
              (match mach#target with
              | Some s -> ["target", s]
              | None -> []
              )
          in
          anchor_link := { 
            h_uri = href;
            h_context = Some mach#base;
            h_method = 
              (try Hyper.parse_method (Html.get_attribute tag "methods")
               with _ -> GET
              );
            h_params = h_params
          };
          in_anchor := true;
          anchor_type := Some HREF;
          fo.start_anchor ();
          (* push_style fo "anchor" *)
        with Not_found ->
          (match !anchor_type with
          | None -> raise (Invalid_Html "Missing NAME or HREF in <A>")
          | _ -> ()
          )
      end
      (*e: [[Html_disp.Make.init.open_anchor()]] look for HREF attribute *)
    in

    let close_anchor fo =
      match !anchor_type with
      (*s: [[Html_disp.Make.init.close_anchor()]] match anchor type cases *)
      | Some NAME ->
          in_anchor := false;
          anchor_type := None
      (*x: [[Html_disp.Make.init.close_anchor()]] match anchor type cases *)
      | Some HREF -> 
          fo.end_anchor !anchor_link;
          (* pop_style fo "anchor"; *)
          in_anchor := false;
          anchor_type := None
      (*e: [[Html_disp.Make.init.close_anchor()]] match anchor type cases *)
      | None -> raise (Invalid_Html "Unmatched </A>")
    in
    mach#add_tag "a" open_anchor close_anchor;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.8 Line break: <BR> 
     *)
    mach#add_tag "br" 
      (fun fo tag -> fo.print_newline true)
      ignore_close
    ;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.9 Horizontal Rule: <HR>
     *)
    mach#add_tag "hr"
      (fun fo tag -> 
         let width =
           try length_of_string (Html.get_attribute tag "width")
           with Not_found -> Nolength 
         in
         let height =
           try int_of_string (Html.get_attribute tag "size")
           with Not_found | Failure "int_of_string" -> 1
         in
         let solid = Html.has_attribute tag "noshade" in
         fo.print_newline false;
         fo.hr width height solid;
         fo.print_newline false)
      ignore_close
    ;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (*
     * 5.10 Image: <IMG>
     *)
    mach#add_tag "img"
      (fun fo tag -> 
         try
           let src = Html.get_attribute tag "src" in
           let align = Html.get_attribute tag "align" in

           let width = 
             try Some (int_of_string (Html.get_attribute tag "width"))
             with Not_found | Failure "int_of_string" -> None
           in
           let height = 
             try Some (int_of_string (Html.get_attribute tag "height"))
             with Not_found | Failure "int_of_string" -> None
           in

           (*s: [[Html_disp.Make.init()]] IMG case, let alt *)
           let alt = 
             try Html.get_attribute tag "alt"
             with Not_found ->
               let image_name = 
                 let pos = 
                   let cpos = ref (String.length src) in
                   try
                     while !cpos > 0 do
                       match src.[!cpos - 1] with
                       | '/' | '\\' (* for f!@#ing DOS users *) -> raise Exit
                       | _ -> decr cpos
                     done;
                     0
                  with Exit -> !cpos
                in
                if pos = String.length src 
                then "IMAGE"
                else String.sub src pos (String.length src - pos)
             in
             Printf.sprintf "[%s]" image_name	 
           in          
           (*e: [[Html_disp.Make.init()]] IMG case, let alt *)
           let w = fo.create_embedded align width height in
           let link = { 
             h_uri = src; 
             h_context = Some mach#base;
             h_method = GET; 
             h_params = []
           } in
           (*s: [[Html_disp.Make.init()]] IMG case, let map *)
           (* some people use both ismap and usemap...
            *  prefer usemap
            *)
           let map =
             try 
               let mapname = get_attribute tag "usemap"  in
               Maps.ClientSide { 
                 h_uri = mapname;
                 h_context = Some mach#base;
                 h_method = GET;
                 h_params = []
               }
             with Not_found -> 
               if !in_anchor 
               then
                 if has_attribute tag "ismap"
                 then Maps.ServerSide !anchor_link
                 else Maps.Direct !anchor_link
               else NoMap
           in
           (*e: [[Html_disp.Make.init()]] IMG case, let map *)
           mach#imgmanager#add_image  
             { embed_hlink = link;
               embed_frame = w;
               embed_context = mach#ctx#for_embed tag.attributes [];
               embed_map = map;
               embed_alt = alt
              }
         with Not_found -> (* only on SRC *)
           raise (Invalid_Html "missing SRC in IMG"))

      ignore_close
    ;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (* FORMS: they are defined elsewhere (html_form) *)
      FormLogic.init mach;
    (* standard basic HTML2.0 initialisation stops here *)
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (* TABLE support *)
    if !attempt_tables 
    then TableLogic.init mach
    else begin
      let behave_as oldtag newtag =
         mach#add_tag newtag
           (fun fo t -> mach#send (OpenTag {tag_name = oldtag; attributes = []}))
           (fun fo ->   mach#send (CloseTag oldtag))
      in
      (* use DL for tables *)
      behave_as "dl" "table";
      mach#add_tag "tr" ignore_open ignore_close;
      behave_as "dt" "th";
      behave_as "dd" "td"
    end;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (* EMBED
     *  The definition is a mix of what was done for earlier versions
     *  of MMM and Netscape Navigator. The reason is to get compatible HTML for
     *  Caml Applets in both browsers.
     *)
    mach#add_tag "embed"
      (fun fo tag -> 
         try
       let link = {
         h_uri = get_attribute tag "src";
         h_method = GET;
         h_context = Some mach#base;
         h_params = []} in
       let width =
         try Some (int_of_string (get_attribute tag "width"))
         with Not_found -> None
       and height =
         try Some (int_of_string (get_attribute tag "height"))
         with Not_found -> None
       and alttxt = get_attribute tag "alt" in

       let fr = fo.create_embedded "" width height in
       mach#add_embedded {
           embed_hlink = link;
           embed_frame = fr;
           embed_context = mach#ctx#for_embed tag.attributes [];
           embed_map = NoMap; (* yet *)
           embed_alt = alttxt
          }
         with
       Not_found ->
         raise (Invalid_Html ("SRC missing in EMBED")))
      ignore_close;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (* Some HTML 3.2 good features *)
    let areas = ref [] in
    let mapname = ref "" in

    mach#add_tag "map"
      (fun fo t ->
           (* the name of the map *)
           let absname = 
             try 
          let name = get_attribute t "name" in
          (* we must get a normalized name here *)
            Hyper.string_of {h_uri = "#"^name; h_context = Some mach#base;
                    h_method = GET; h_params = []}
             with
           Not_found -> 
             Hyper.string_of (Hyper.default_link mach#base)
           in
           mapname := absname;
           areas := [];
           mach#add_tag "area" 
          (fun fo tag -> 
             let shape = String.lowercase (get_attribute tag "shape")
             and href = 
           try Some (get_attribute tag "href") with Not_found -> None
                 and coords =
           try Maps.parse_coords (get_attribute tag "coords")
           with _ -> [] 
                 and alttxt =
                   try get_attribute tag "alt" with Not_found -> ""
                 in
             let h_params =
           try ["target", get_attribute tag "target"]
           with
             Not_found ->
               match mach#target with
                 Some s -> ["target", s]
               |	None -> []
             in
                 match href with
           None -> () (* this creates a HOLE. not yet supported *)
             | Some uri ->
            let link = {h_uri = uri; h_context = Some mach#base;
                    h_method = GET; h_params = h_params} in
                    let area = 
              match shape with
               "default" -> {area_kind = Default; area_coords = [];
                     area_link = link; area_alt = alttxt}
             | "rect" -> {area_kind = Rect; area_coords = coords;
                  area_link = link; area_alt = alttxt}
             | "circle" -> {area_kind = Circle; area_coords = coords;
                    area_link = link; area_alt = alttxt}
             | "poly" -> {area_kind = Poly; area_coords = coords;
                  area_link = link; area_alt = alttxt} 
             | _ -> {area_kind = Default; area_coords = [];
                  area_link = link; area_alt = alttxt} in
                    areas := area :: !areas)
              ignore_close)

      (fun fo -> 
        mach#remove_tag "area";
        Maps.add !mapname !areas)
    ;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    mach#add_tag "basefont"
      (fun fo t -> 
         try
           let n = int_of_string (get_attribute t "size") in
           fo.set_defaults "font" [Font (FontIndex n)]
         with Not_found | Failure "int_of_string" ->
           raise (Invalid_Html "invalide SIZE"))
      ignore_close;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    let fontchanges = ref [] in

    mach#add_tag "font"
      (fun fo t ->
         let attrs = [] in
         let attrs =
           try
             let size = get_attribute t "size" in
             let l = String.length size in
             if l = 0 
             then raise Not_found
             else 
               if size.[0] = '+' 
               then
                 (Font (FontDelta (int_of_string (String.sub size 1 (pred l)))))
                        :: attrs
               else 
                 if size.[0] = '-' 
                 then (Font (FontDelta (int_of_string size)))::attrs
                 else (Font (FontIndex (int_of_string size)))::attrs
           with Not_found | Failure _ -> attrs 
         in
         let attrs = 
            try
              let color = get_attribute t "color" in
              (FgColor color)::attrs
            with Not_found -> attrs 
         in
         (* attrs may well be the empty list *)
         if attrs <> [] 
         then fo.push_attr attrs;
         fontchanges := attrs :: !fontchanges)

      (fun fo -> 
         match !fontchanges with
         | [] -> raise (Invalid_Html "unmatched </font>")
         | x::l -> 
             fontchanges := l;
             if x <> [] 
             then fo.pop_attr x)
    ;
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (* Some HTML 3.2 obnoxious features *)
    (* STYLE, SCRIPT in HEAD: not managed here
       For some reason, script is also allowed in text by the DTD.
       Make sure we just dump the contents...
       We do the same for style, just in case people dont respect the DTD
     *)
    mach#add_tag "style"
      (fun fo t -> mach#push_action (fun s -> ()))
      (fun fo -> mach#pop_action);
    (*x: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
    (* Some HTML 3.2 obnoxious features *)
    mach#add_tag "script"
      (fun fo t -> mach#push_action (fun s -> ()))
      (fun fo -> mach#pop_action);
    (*e: [[Html_disp.Make.init()]] HTML elements machine initialisation *)
  ()
  (*e: function Html_disp.Make.init *)

  (*s: function Html_disp.Make.create *)
  let create (ctx, imgmanager) =
    let mach = new display_machine (ctx, imgmanager) in
    init mach;
    (*s: [[Html_disp.Make.create()]] run user hooks *)
    !user_hooks |> List.iter (fun f -> f (mach :> machine));
    (*e: [[Html_disp.Make.create()]] run user hooks *)
    (mach :> machine)
  (*e: function Html_disp.Make.create *)
end
(*e: functor Html_disp.Make *)
(*e: ./display/html_disp.ml *)

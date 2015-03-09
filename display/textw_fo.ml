(*s: ./display/textw_fo.ml *)
open Printf
open Tk
open Frx_text
open Hyper
open Viewers
open Html
open Htmlfmt
open Fonts


(* Text widget formatter for the HTML Display Machine
 * The main function builds a GfxHTML, in two cases
 *    1- normal (viewing an HTML document)
 *    2- nested (a cell in a table)
 *    3- 
 *)

(*s: constant Textw_fo.html_bg *)
(* Default background and foreground colors *)
let html_bg = ref "white"
(*e: constant Textw_fo.html_bg *)
(*s: constant Textw_fo.html_fg *)
let html_fg = ref "black"
(*e: constant Textw_fo.html_fg *)

(*s: constant Textw_fo.usecolors *)
(* Preference settings *)
let usecolors = ref true     (* use colors (fg/bg) specified in document *)
(*e: constant Textw_fo.usecolors *)
(*s: constant Textw_fo.internal_buffer *)
let internal_buffer = ref 4000
(*e: constant Textw_fo.internal_buffer *)

(*s: function Textw_fo.create *)
(* Build a formatter, as required by html_disp *)
let create namer spec top ctx =
  (*s: [[Textw_fo.create]] locals *)
  let other_bg = ref (fun _ -> ()) in
  let fhtml, thtml =
    match spec with
      TopFormatter pscrolling ->
    let f,t = 
     if pscrolling then begin
      let f,t = 
        Ctext.create top [Wrap WrapWord; State Disabled] true in
        Canvas.configure (Winfo.parent t)
           [Background (NamedColor !html_bg)];
         other_bg := Canvas.configure (Winfo.parent t);
        f, t
      end
     else
       new_scrollable_text top 
          [Wrap WrapWord; State Disabled]
          true 
    in
    (* Try to solve focus problem -- JPF *)
    bind t [[],Enter] (BindSet ([], fun _ -> Focus.set t));
    f, t
    | NestedFormatter -> (* Embedded formatters (tables) *)
       let t = Text.create_named top (namer())
            [BorderWidth (Pixels 0); State Disabled; 
              Relief Sunken; Wrap WrapNone;
          TextWidth 1; TextHeight 1]
    in
        t, t
    | FrameFormatter args -> 
    let marginwidth = 
      try [PadX (Pixels (int_of_string (List.assoc "marginwidth" args)))]
      with Not_found | Failure "int_of_string" -> []
    and marginheight = 
      try [PadY (Pixels (int_of_string (List.assoc "marginheight" args)))]
      with Not_found | Failure "int_of_string" -> []
    in
   let f,t = 
     Ctext.create top (marginwidth @ marginheight @ 
                [TextHeight 1; 
                  Wrap WrapWord; State Disabled]) true in
   Canvas.configure (Winfo.parent t)
     [Background (NamedColor !html_bg)];
         other_bg := Canvas.configure (Winfo.parent t);

    f, t
  in

  (* Tk4.0pl3 fix, + avoid cb to scrollbar *)
  (* Make the widget searchable *)
  (* NOTE: search doesn't apply to nested windows *)
  begin match spec with
    TopFormatter _ | FrameFormatter _ -> 
      Text.configure thtml [TakeFocus true; InsertOffTime 0];
      Frx_text.addsearch thtml
  | NestedFormatter -> 
      Text.configure thtml [TakeFocus false; InsertOffTime 0]
  end;

  (* Set (other) defaults *)
  let _, html_font = Fonts.compute_tag !Fonts.default in
   Text.configure thtml html_font;

  (* transparent GIF hack *)
  Textvariable.set (Textvariable.coerce "TRANSPARENT_GIF_COLOR") !html_bg;

  (* The formatter
   *    to minimize calls to Tk, we write only one string for 
   * each paragraph larger than some size. Because of this, it seems
   * that we also have to set tags and marks at the end.
   *)

  (* Things queued *)
  let marks = ref []
  and embedded = ref []
  and tagdefs = new Attrs.tags thtml
  (* Hypertext Anchor support *)
  and anchors = new Attrs.anchortags thtml

  (* It's easier for us to keep character positions as offsets from the
   * beginning, but it's very costly in Tk (conversion 0+nchars -> index),
   * especially when the size gets large.
   * Thus, we keep the base index of the current buffer, and positions
   * as offsets from there.
   * We must be careful not to leave position values relative to old
   * buffer_base.
   *)
  and buffer_base = ref (LineChar(0,0))
  and position = ref 0
  and anchor_start = ref (TextIndex(LineChar(0,0),[]))

  (* Paragraphs and space squeezing *)
  and trailing_space = ref false
  and prev_is_newline = ref false
      (* if this is false, we are displaying text. if this is true, we
         just issued a newline *)
  in

  (* Index for Tk *)
  let get_index p = TextIndex (!buffer_base, [CharOffset p]) in
  let cur () = get_index !position in

  (* colors for *this* window, can be changed by set_defaults *)
  let fg = ref !html_fg
  and bg = ref !html_bg
  in
  (* inherited properties (set_defaults) : we apply them to embedded
     formatters (table cells) *)
  let inherited = ref [] in
  anchors#init ctx;      (* install bindings *)
  anchors#define "visited" [Foreground (NamedColor "MidnightBlue")];
  anchors#define "anchor" [Foreground (NamedColor "#0000ff"); Underline true];


  (* Size of buffer can impact performances *)
  let refresh_threshold = 
     if !internal_buffer < 1000 then 1000 else !internal_buffer in
  let buffer = Ebuffer.create (2 * refresh_threshold)
  and last_flush = ref !Low.global_time in

  let internal_flush refresh = (* flush the buffer *)
    last_flush := !Low.global_time;
    Text.configure thtml [State Normal];
    Text.insert thtml textEnd (Ebuffer.get buffer) [];
    Ebuffer.reset buffer;
    List.iter 
      (function (opts,p) -> Text.window_create thtml (get_index p) opts)
      (List.rev !embedded);
    List.iter (function (m,p) -> Text.mark_set thtml m (get_index p)) !marks;
    tagdefs#flush;
    anchors#flush;
    Text.configure thtml [State Disabled];
    marks := [];
    embedded := [];
     (* reset the position *)
    buffer_base := 
       Text.index thtml (TextIndex(!buffer_base,[CharOffset !position]));
    position := 0;
     (* try to give a reasonable initial height for the text widget *)
    if refresh then begin 
     begin match spec with
       TopFormatter true 
     | NestedFormatter 
     | FrameFormatter _ -> Fit.set_initial_height thtml
     | _ -> ()
     end;
     Low.update_idletasks()
    end
     in

  let put_text s =
    match String.length s with
      0 -> ()
    | l ->
        position := !position + Lexkanji.length s;
        prev_is_newline := false;
        Ebuffer.output_string buffer s;
    trailing_space := s.[l-1] = ' ';
    if  !Low.global_time > !last_flush + 4 (* it's been a while *)
    then internal_flush true
    else if  Ebuffer.used buffer > refresh_threshold 
    then internal_flush false
       in

  (* Logic for tag manipulation *)
  let margins = new Attrs.margin tagdefs
  and aligns = new Attrs.align tagdefs
  and fonts = new Attrs.font tagdefs
  and fgcolors = new Attrs.fgcolor tagdefs
  and bgcolors = new Attrs.bgcolor tagdefs
  and spacing = new Attrs.spacing tagdefs
  and offset = new Attrs.offset tagdefs
  and underline = new Attrs.misc (tagdefs, "underline", [Underline true])
  and strike = new Attrs.misc (tagdefs, "strike", [OverStrike true])

  in
  let put_embedded w align =
    let opts = match String.lowercase align with
      "top" -> [Align Align_Top]
    | "middle" -> [Align Align_Center] (* not exactly *)
    | "bottom" -> [Align Align_Baseline] 
    |  _ -> [] in
    embedded := ((Window w)::opts, !position) :: !embedded;
    prev_is_newline := false;
    incr position    (* an embedded window is one char wide *)    
  in

  let break () =
     if not !prev_is_newline then begin
       put_text "\n"; prev_is_newline := true
       end
  in

  let paropen = ref (cur()) in
  (*e: [[Textw_fo.create]] locals *)
  let formatter =           
  { 
    (*s: function Textfw_fo.create.new_paragraph *)
    new_paragraph = (fun () -> 
      break(); 
      spacing#push (cur()) 5; 
      paropen := cur()
    );
    (*e: function Textfw_fo.create.new_paragraph *)
    (*s: function Textfw_fo.create.close_paragraph *)
    close_paragraph = (fun () -> 
      spacing#pop (cur()) 5; 
      if (cur() = !paropen) 
      then prev_is_newline := false;
      break()
    );
    (*e: function Textfw_fo.create.close_paragraph *)
    (*s: function Textfw_fo.create.print_newline *)
    print_newline = (fun force -> 
     if force then begin
       put_text "\n"; 
       trailing_space := true
     end
     else break()
    );
    (*e: function Textfw_fo.create.print_newline *)
    (*s: function Textfw_fo.create.print_verbatim *)
    print_verbatim = (fun s -> 
      put_text s; 
      prev_is_newline := false
    );
    (*e: function Textfw_fo.create.print_verbatim *)
    (*s: function Textfw_fo.create.format_string *)
    format_string = (fun s -> 
      if not !prev_is_newline 
      then (* we are in text *)
        put_text (Html.beautify !trailing_space s)
      else (* decide if we should start a text *)
        let bs = Html.beautify true s in
        if bs = "" 
        then () (* it was all spaces *)
        else begin
          put_text bs;
          prev_is_newline := false
        end
    );
    (*e: function Textfw_fo.create.format_string *)

    (*s: function Textfw_fo.create.flush *)
    flush = (fun () -> 
      fonts#pop_all (cur());	(* basefont lossage *)
      internal_flush true
    );
    (*e: function Textfw_fo.create.flush *)

    (*s: function Textfw_fo.create.hr *)
    hr = begin
      let hrsym = Mstring.egensym "hr" in
      (fun width height solid ->
        let fr = Hr.create_named thtml (hrsym()) width height solid in
        Frame.configure fr [Background (NamedColor !fg)];
        put_embedded fr ""
      )
    end;
    (*e: function Textfw_fo.create.hr *)
    (*s: function Textfw_fo.create.bullet *)
    (* TODO *)
    bullet = begin
     let bulletsym = Mstring.egensym "bullet" in
     (fun s -> 
       try 
         let img = Hashtbl.find Attrs.bullet_table s in
         put_embedded (Label.create_named thtml (bulletsym())
                      [img; BorderWidth (Pixels 0);
                       Background (NamedColor !html_bg)]) ""
       with Not_found  -> put_text "*"
      )
    end;
    (*e: function Textfw_fo.create.bullet *)


    (*s: function Textfw_fo.create.set_defaults *)
    (* TODO : vlink *)
    set_defaults = (fun name attrs -> 
      inherited := (name, attrs) :: !inherited;
      match name with
      | "background" ->
          attrs |> List.iter (function
          | BgColor s ->
              if !usecolors then
                let c = Attrs.html_color s in
                if Frx_color.check c then begin
                  bg := c;
                  Resource.add 
                     (sprintf "Mmm%s*background" (Widget.name thtml))
                       c Interactive;
                  Text.configure thtml [Background (NamedColor c)];
                  !other_bg [Background (NamedColor c)]
                end
          | _ -> ())

      | "foreground" ->
          attrs |> List.iter (function
            | FgColor s ->
               if !usecolors then
                 let c = Attrs.html_color s in
                 if Frx_color.check c then begin
                   fg := c;
                   Resource.add 
                     (sprintf "Mmm%s*foreground" (Widget.name thtml))
                       c Interactive;
                   Text.configure thtml [Foreground (NamedColor c)]
               end
            | _ -> ())

      | "link" ->
          attrs |> List.iter (function
            | FgColor s ->
                if !usecolors then
                  let c = Attrs.html_color s in
                  if Frx_color.check c then 
                    anchors#change "anchor" [Foreground (NamedColor c)]
            | _ -> ())

      | "alink" ->
          attrs |> List.iter (function
            | FgColor s ->
                if !usecolors then
                  let c = Attrs.html_color s in
                  if Frx_color.check c then 
                    anchors#change "visited" [Foreground (NamedColor c)]
            | _ -> ())

      | "font" ->
          attrs |> List.iter (function
            | Font (FontIndex x)  -> 
                fonts#set_base (cur()) x
            |	_ -> ())

      | _ -> ()
    );
    (*e: function Textfw_fo.create.set_defaults *)

    (*s: function Textfw_fo.create.push_attr *)
    push_attr = (fun l ->
      let fis = ref [] in
      l |> List.iter (function
        | Font fi -> 
            fis := fi :: !fis
        | Margin n -> 
            margins#push (cur()) n
        | Justification a -> 
            aligns#push (cur()) a
        | FgColor s -> 
            if !usecolors 
            then fgcolors#push (cur()) s
        | BgColor s ->
            if !usecolors 
            then bgcolors#push (cur()) s
        | Spacing n -> 
            spacing#push (cur()) n
        | Underlined -> 
            underline#push (cur())
        | Striked -> 
            strike#push (cur())
        | Superscript -> 
            fis := (FontDelta (-2)) :: !fis;
            offset#push (cur()) 5
        | Lowerscript ->
            fis := (FontDelta (-2)) :: !fis;
            offset#push (cur()) (-5)
        );
      if !fis <> [] 
      then fonts#push (cur()) !fis;
    );
    (*e: function Textfw_fo.create.push_attr *)
    (*s: function Textfw_fo.create.pop_attr *)
    pop_attr = (fun l ->
      let fis = ref [] in
      l |> List.iter (function
        | Font fi -> fis := fi :: !fis
        | Margin n -> margins#pop (cur()) n
        | Justification a -> aligns#pop (cur()) a
        | FgColor s -> 
             if !usecolors then fgcolors#pop (cur()) s
        | BgColor s ->
             if !usecolors then bgcolors#pop (cur()) s
        | Spacing n -> spacing#pop (cur()) n
        | Underlined -> underline#pop (cur())
        | Striked -> strike#pop (cur())
        | Superscript ->
                  fis := (FontDelta (-2)) :: !fis;
                  offset#pop (cur()) 5
        | Lowerscript ->
                  fis := (FontDelta (-2)) :: !fis;
                  offset#pop (cur()) (-5)
      );
      if !fis <> [] 
      then fonts#pop (cur()) !fis;
    );
    (*e: function Textfw_fo.create.pop_attr *)

    (*s: function Textfw_fo.create.isindex *)
    (* Compliance: text is not part of document ? *)
    isindex = (fun prompt base ->
      let f,e = Frx_entry.new_label_entry thtml prompt
          (function s -> 
            ctx#goto { h_uri = "?" ^ Urlenc.encode s;
                       h_context = Some base;
                       h_method = GET;
                       h_params = []}
      ) in
      (* default size 0 ! *)
      Entry.configure e [TextWidth 20];
      put_embedded f "";
      put_text "\n"
    );
    (*e: function Textfw_fo.create.isindex *)
    
    (*s: function Textfw_fo.create.start_anchor *)
    start_anchor = (fun () -> anchor_start := (cur()));
    (*e: function Textfw_fo.create.start_anchor *)
    (*s: function Textfw_fo.create.end_anchor *)
    (* set the tag for the anchor *)
    end_anchor = (fun link -> anchors#add_anchor !anchor_start (cur()) link);
    (*e: function Textfw_fo.create.end_anchor *)

    (*s: function Textfw_fo.create.add_mark *)
    (* WARNING: if anchor name is a standard tk name, such as end,
       we're f*cked, so we force # *)
    add_mark = (fun s -> marks := ("#"^s, !position) :: !marks );
    (*e: function Textfw_fo.create.add_mark *)

    (*s: function Textfw_fo.create.create_embedded *)
    create_embedded = begin
      (* avoid space leak in Tk hash table : reuse the same names *)
      let embsym = Mstring.egensym "emb" in

      (fun a w h ->
         let f = Frame.create_named thtml (embsym()) [Class "HtmlEmbedded"] in
         if !usecolors 
         then Frame.configure f [Background (NamedColor !bg)];

         (* To solve the focus problem 
         Tk.bindtags  f ((WidgetBindings thtml) :: Tk.bindtags_get f);
         bind f [[],Enter] (BindSet ([], fun _ -> Focus.set thtml)); 
          *)
         (* -- end *)
         (match w, h with
          | Some w, Some h ->
              Frame.configure f [Width (Pixels w); Height (Pixels h);
              BorderWidth (Pixels 0)];
               Pack.propagate_set f false
          | _, _ -> ()
         );
         put_embedded f "";
         f
      )
    end;
    (*e: function Textfw_fo.create.create_embedded *)

    (*s: function Textfw_fo.create.see_frag *)
    (* we try to remember the last "reading" position, so you can easily
     * switch back from a goto to some particular place in the document
     *)
    see_frag = begin
      let prev_frag = ref false
      and view_mem = ref 0.0 in
      match spec with
      | TopFormatter true -> (* this is pscrolling mode *)
        (function
         | None -> (* no place in particular *)
             if !prev_frag then begin
               try Canvas.yview (Winfo.parent thtml) (MoveTo !view_mem)
               with Protocol.TkError _ -> ()
             end;
             prev_frag := false
        | Some s ->
            if not !prev_frag then begin
              try view_mem := fst (Canvas.yview_get (Winfo.parent thtml))
              with Protocol.TkError _ -> ()
            end;
            prev_frag := true;
            if s <> "" then
              try
                let _,y,_,_,_  = Text.dlineinfo thtml
                     (TextIndex (Mark ("#"^s), [LineOffset (-2)]))
                and _,ye,_,_,_ = Text.dlineinfo thtml 
                     (TextIndex (End, [CharOffset (-1)])) in
               Canvas.yview (Winfo.parent thtml) 
                    (MoveTo (float y /. float ye))
              with Protocol.TkError _ -> ()
      )
      |	_ ->
        (function
        | None -> (* no place in particular *)
            if !prev_frag then begin
              (* we were at view_mem *)
              try Text.yview thtml (MoveTo !view_mem)
              with Protocol.TkError _ -> ()
            end;
            prev_frag := false
        | Some s -> (* go to s *)
            if not !prev_frag then begin
              (* we were not in some special place, remember it *)
              try view_mem := fst (Text.yview_get thtml)
              with Protocol.TkError _ -> ()
            end;
            prev_frag := true;
            if s <> "" then
              try Text.yview_index thtml 
                 (TextIndex (Mark ("#"^s), [LineOffset (-1)]))
              with Protocol.TkError _ -> ()
        )
    end
    (*e: function Textfw_fo.create.see_frag *)
    } in

  formatter, fhtml
(*e: function Textw_fo.create *)
(*e: ./display/textw_fo.ml *)

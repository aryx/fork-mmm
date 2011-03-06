(* A trick by Steve Ball to do pixel scrolling on text widgets *)
(* USES frx_fit *)
open Tk

let tag_name = "CTEXT_RO"

let navigation_keys tx =
  let tags = bindtags_get tx in
    match tags with
      (WidgetBindings t)::l when t = tx ->
	Canvas.configure (Winfo.parent t) [YScrollIncrement (Pixels 15)];
      	bindtags tx ((WidgetBindings tx) :: (TagBindings tag_name) :: l)
    | _ -> ()


let create top opts navigation =
  let f = Frame.create_named top "smoothf" [] in
  let lf = Frame.create_named f "left" [] in
  let rf = Frame.create_named f "right" [] in
  let c = Canvas.create_named lf "smoothc" [BorderWidth (Pixels 0); TakeFocus true]
  and xscroll = Scrollbar.create_named lf "x" [Orient Horizontal]
  and yscroll = Scrollbar.create_named rf "y" [Orient Vertical] 
  and secret = Frame.create_named rf "secret" []
  in
  (* automatic scrollbars *)
  let has_x = ref false
  and has_y = ref false
  in
  let putx () = 
    pack [xscroll] [Before c; Side Side_Bottom; Fill Fill_X];
    pack [secret] [Before yscroll; Side Side_Bottom];
    has_x := true
  and remx () = 
    Pack.forget [xscroll; secret];
    has_x := false
  and puty () = 
    pack [rf] [Before lf; Side Side_Right; Fill Fill_Y];
    has_y := true
  and remy () = Pack.forget [rf]; has_y := false
  in

  let wrap_scroll isthere put rem scrollcmd   =
    fun first last ->
      scrollcmd first last;
      if !isthere then 
      	if first = 0.0 && last = 1.0 then rem() else ()
      else
      	if first <> 0.0 || last <> 1.0 then put() else ()
  in
  let t = Text.create_named c "smootht" (BorderWidth(Pixels 0) :: opts) in
    if navigation then navigation_keys t;
 
    (* Make the text widget an embedded canvas object *)
    ignore (
      Canvas.create_window c (Pixels 0) (Pixels 0)
      	[Anchor NW; Window t; Tags [Tag "main"]]);
    Canvas.focus c (Tag "main");
    Canvas.configure c 
    [YScrollCommand (wrap_scroll has_y puty remy (Scrollbar.set yscroll))];
    (* The horizontal scrollbar is directly attached to the
     * text widget, because h scrolling works properly *)
    Scrollbar.configure xscroll [ScrollCommand (Text.xview t)];
    (* But vertical scroll is attached to the canvas *)
    Scrollbar.configure yscroll [ScrollCommand (Canvas.yview c)];
    let scroll, check = Fit.vert t in
    Text.configure t [
      	XScrollCommand (wrap_scroll has_x putx remx (Scrollbar.set xscroll));
        YScrollCommand (fun first last ->
      	   scroll first last;
	   let x,y,w,h = Canvas.bbox c [Tag "main"] in
	     Canvas.configure c 
      	       [ScrollRegion (Pixels x, Pixels y, Pixels w, Pixels h)]);
        ];
    (* B2 Scrolling : based on std script text.tcl 
     * Since t has the focus, it will handle the event, even if we play
     * with bindtags. Thus ev_MouseX/Y are given in the "full" text.
     * Moreover, as the text scrolls, the re-positionning of the text
     * affects the event fields (non-monotonicity !)
     * However, since "scan" is interested only in relative positions,
     * we can use root coordinates directly
     *)
    let x = ref 0
    and y = ref 0
    in
    bind t [[], ButtonPressDetail 2]
      (BindSetBreakable ([Ev_RootX; Ev_RootY],
		(fun ei -> 
		  x := ei.ev_RootX;
		  y := ei.ev_RootY;
		  Canvas.scan_mark c !x !y)));
    bind t [[Button2], Motion]
      (BindSetBreakable ([Ev_RootX; Ev_RootY],
		(fun ei -> 
		  let dx = ei.ev_RootX
		  and dy = ei.ev_RootY
		  in 
		  if dx <> !x || dy <> !y then
		    Canvas.scan_dragto c dx dy)));

    bind c [[],Configure] (BindSet ([Ev_Width], (fun ei ->
      Canvas.configure_window c (Tag "main") [Width (Pixels ei.ev_Width)])));

    pack [c] [Side Side_Left; Fill Fill_Both; Expand true];
    pack [lf] [Side Side_Left; Fill Fill_Both; Expand true];
    (* pack [secret] [Side Side_Bottom]; *)
    pack [yscroll] [Side Side_Top; Fill Fill_Y; Expand true];
    (* pack [rf] [Side Side_Right; Fill Fill_Y]; *)
    (* pack [xscroll] [Side Side_Bottom; Fill Fill_X]; *)
    f, t


(* Same as frx_text, but work on canvas instead of text for scrolling *)
let page_up tx   =  Canvas.yview (Winfo.parent tx) (ScrollPage (-1))
and page_down tx =  Canvas.yview (Winfo.parent tx) (ScrollPage 1)
and line_up tx   =  Canvas.yview (Winfo.parent tx) (ScrollUnit (-1))
and line_down tx =  Canvas.yview (Winfo.parent tx) (ScrollUnit 1)
and top tx = Canvas.yview (Winfo.parent tx) (MoveTo 0.0)
and bottom tx = Canvas.yview (Winfo.parent tx) (MoveTo 1.0)

(* We use Mod1 instead of Meta or Alt *)
let init () = 
  List.iter (function ev ->
      	     tag_bind tag_name ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> page_up ei.ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "BackSpace"];
	    [[], KeyPressDetail "Delete"];
	    [[], KeyPressDetail "Prior"];
	    [[], KeyPressDetail "b"];
	    [[Mod1], KeyPressDetail "v"]
	   ];
  List.iter (function ev ->
      	     tag_bind tag_name ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> page_down ei.ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "space"];
	    [[], KeyPressDetail "Next"];
	    [[Control], KeyPressDetail "v"]
	   ];
  List.iter (function ev ->
      	     tag_bind tag_name ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> line_up ei.ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "Up"];
	    [[Mod1], KeyPressDetail "z"]
	   ];
  List.iter (function ev ->
      	     tag_bind tag_name ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> line_down ei.ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "Down"];
	    [[Control], KeyPressDetail "z"]
	   ];

  List.iter (function ev ->
      	     tag_bind tag_name ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> top ei.ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "Home"];
	    [[Mod1], KeyPressDetail "less"]
	   ];

  List.iter (function ev ->
      	     tag_bind tag_name ev 
      	       	  (BindSetBreakable ([Ev_Widget], 
      	       	       	       	 (fun ei -> bottom ei.ev_Widget; break()))))
	   [
	    [[], KeyPressDetail "End"];
	    [[Mod1], KeyPressDetail "greater"]
	   ]

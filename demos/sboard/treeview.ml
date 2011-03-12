(*
 * THIS FILE CONTAINS SOME REALLY DIRTY AND NASTY CODES FOR TEXT WIDGET.
 *   I DON'T WANT TO EXPLAIN HOW I DID IT.
 *                                                                    JPF
 *)

open Tk
open Tree

module Version = Version_sboard

exception FoundItem of item

let viewers = ref [] (* : treeview list ref *)

let redraw_for_all_viewers item =
  let root = Tree.root item in
  List.iter (fun vw ->
    if vw#tree#root == root then
      if vw#is_visible item then vw#redraw_item item) !viewers

let insert_for_all_viewers item =
  let root = Tree.root item in
  List.iter (fun vw ->
    if vw#tree#root == root then
      if vw#is_visible item then vw#insert_item item) !viewers

let erase_for_all_viewers item =
  let root = Tree.root item in
  List.iter (fun vw ->
    if vw#tree#root == root then
      if vw#is_visible item then vw#erase_item item) !viewers

let create new_instance_fun tree =
  let top = Toplevel.create Widget.default_toplevel [Class "SurfView"] in
  Wm.withdraw top;
  Wm.title_set top "Surfboard";
  let tv = new_instance_fun (top, tree) in
  tv#init;
  tv#display tree tree#root;
  Wm.deiconify top

exception Excp of string

let edit i =
  try
    let t = Toplevel.create Widget.default_toplevel [Class "SurfEdit"] in
    try
      let f_title = Frame.create t [BorderWidth (Pixels 1); Relief Raised]
      and info = Frame.create t [BorderWidth (Pixels 1); Relief Raised]
      and buttons = Frame.create t [BorderWidth (Pixels 1); Relief Raised]
      in
      let titlev = Textvariable.create_temporary t in
        Textvariable.set titlev i#name;
      let icon = Label.create f_title [(************)]
      and title = Entry.create f_title [ TextVariable titlev;
					 BorderWidth (Pixels 1);
					 TextWidth 40]
      in
      pack [icon] [Side Side_Left; Fill Fill_Y; Anchor W];
      pack [title] [Side Side_Left; Fill Fill_X; Expand true; Anchor W];
      pack [f_title] [Side Side_Top; Fill Fill_X];
      let modify =
	begin match i#itemtype with
	  Page ->
	    let urlf = Frame.create info [] in
	    let urlv = Textvariable.create_temporary t in
	    Textvariable.set urlv i#page#url;
	    let lab = Label.create urlf [Text "URL:"; TextWidth 5]
	    and ent = Entry.create urlf [ TextVariable urlv;
					  BorderWidth (Pixels 1);
					  TextWidth 40]
	    in
	    let txt = Text.create info [ BorderWidth (Pixels 1);
					 Relief Sunken;
					 TextWidth 40;
					 TextHeight 5 ]
	    in
	    pack [lab] [Side Side_Left; Anchor W];
	    pack [ent] [Side Side_Left; Fill Fill_X; Expand true;
			 Anchor W];
	    pack [urlf] [Side Side_Top; Anchor NW; Fill Fill_X];
	    pack [txt] [Side Side_Top; Fill Fill_X; Expand true;
			 Anchor W];
	    Text.insert txt (TextIndex (LineChar (1,0),[])) i#page#comment [];
	    begin fun () ->
	      i#page#set_title (Textvariable.get titlev);
	      i#page#set_url   (Textvariable.get urlv);
	      i#page#set_comment (Text.get txt (TextIndex (LineChar (1,0),[]))
				                (TextIndex (End, [])));
	      redraw_for_all_viewers i
            end
	  | Dir ->
	      let txt = Text.create info [ BorderWidth (Pixels 1);
					   Relief Sunken;
					   TextWidth 40;
					   TextHeight 5 ]
	      in
	      pack [txt] [Side Side_Top; Fill Fill_X; Expand true;
			   Anchor W];
	      Text.insert txt (TextIndex (LineChar (1,0),[])) i#dir#comment [];
	      begin fun () ->
		i#dir#set_title (Textvariable.get titlev);
		i#dir#set_comment (Text.get txt (TextIndex (LineChar (1,0),[]))
			                   (TextIndex (End, [])));
		(* GUI refresh *)
		redraw_for_all_viewers i
              end
	  | Separator -> (fun () -> ()) (* it is not editable *)
	end
      in
      pack [info] [Fill Fill_Both; Expand true];

      let okb = Button.create buttons [
	Text "Ok";
	Command (fun () -> modify (); destroy t)]
      and canb = Button.create buttons [Text "Cancel"; Command (fun () ->
	destroy t)]
      in
      pack [okb] [Side Side_Left;
		   PadX (Millimeters 2.0);
		   PadY (Millimeters 2.0)];
      pack [canb] [Side Side_Right;
		    PadX (Millimeters 2.0);
		    PadY (Millimeters 2.0)];
      pack [buttons] [Fill Fill_X]
    with
      Protocol.TkError e -> raise (Excp e)
  with
    Excp e -> raise (Protocol.TkError e)
  | Protocol.TkError _ -> (* failed to create "edit" *)
      Bell.ring ()

type widgets = {
    text : Widget.widget;
    dirmenub : Widget.widget;
    dirmenu : Widget.widget;
    filemenu : Widget.widget;
    editmenu : Widget.widget
  }

type 'a t_treeview = 'a
 constraint 'a = <
  coerce : _;
  get_tag : _;
  tree : _;

  reset_select : _;

  select : _;
  deselect : _;
  highlight : _;
  is_selected : _;

  is_opened : _;

  open_dir : _;

  close_dir : _;

  visible_descendant : _;

  depth_of_item : _;

  line_of_item : _;

  item_of_line : _;

  insert_item : _;

  erase_item : _;

  redraw_item : _;

  display : _;

  check_sanity : _;

  is_visible : _;
	
  popup : _;

  init : _;

  close : _;

  is_my_text : _;

>;;

class treeview (top, (tree : Tree.tree)) = object (self)

(*  val top = top*)

  method coerce : treeview = (self :> _ t_treeview)

  val widgets =

  let menu = Frame.create top [BorderWidth (Pixels 1); Relief Raised] in
  let mfile = Menubutton.create menu [Text "File"] in
  let filemenu = Menu.create mfile [TearOff false] in
  Menubutton.configure mfile [Menu filemenu];

  Menu.add_command filemenu [ Label "About"; Command Version.about ];

  let medit = Menubutton.create menu [Text "Edit"] in
  let editmenu = Menu.create medit [TearOff false] in
  Menubutton.configure medit [Menu editmenu];

  let dirmenub = Menubutton.create top [TextWidth 30; Text "???";
			       BorderWidth (Pixels 1); Relief Raised] in
  let dirmenu = Menu.create dirmenub [TearOff false] in
  Menubutton.configure dirmenub [Menu dirmenu];
  let frame = Frame.create top [] in
  let text = Text.create frame [ TextWidth 30;
				 Cursor (XCursor "top_left_arrow");
				 Wrap WrapNone;
				 State Disabled;
				 ExportSelection false;
				 Background (NamedColor "#D8D8D8");
				 SelectBackground (NamedColor "#D8D8D8");
				 SelectBorderWidth (Pixels 0) ]
  in
  Text.tag_configure text "selected" [Background (NamedColor "#FFFFCF")];
  Text.tag_configure text "dirline" [Relief Raised; BorderWidth (Pixels 1)];
  Text.tag_configure text "dirbutton" [Relief Raised; BorderWidth (Pixels 2)];

  Text.tag_configure text "normal" [];

  (* Scroll *)
  let scrbar = Scrollbar.create frame [] in
  Text.configure text [YScrollCommand (Scrollbar.set scrbar)];
  Scrollbar.configure scrbar [ScrollCommand (Text.yview text)];

  (* Pack *)
  pack [mfile; medit] [Anchor W; Side Side_Left];
  pack [scrbar] [Side Side_Right; Fill Fill_Y];
  pack [text] [Side Side_Left; Fill Fill_Both; Expand true];
  pack [menu; dirmenub] [Fill Fill_X];
  pack [frame] [Fill Fill_Both; Expand true];

  { text= text;
    filemenu= filemenu;
    editmenu= editmenu;
    dirmenub= dirmenub;
    dirmenu= dirmenu
  }

  val tagtable = Hashtbl.create 31

  method get_tag depth =
    let gray depth =
      (* maxdepth = 10
	 darkest  = 0x20
	 level0   = 0xd8 *)
      let depth = if depth > 16 then 16 else depth in
      let x = 0xd8 - (0xd8 - 0x20) * depth / 10 in
      NamedColor (Printf.sprintf "#%02X%02X%02X" x x x)
    in
    try Hashtbl.find tagtable depth
    with
      Not_found ->
	let name = "level"^string_of_int depth in
	Hashtbl.add tagtable depth name;
	Text.tag_configure widgets.text name [Background (gray depth)];
	(* we must be bottom *)
	Text.tag_lower_bot widgets.text name;
	name

  val mutable topdir = tree#root

  val mutable tree = tree
  method tree = tree

  (*************************************************************** selection *)
  val selected = (Hashtbl.create 107 : (item, unit) Hashtbl.t)

  method reset_select =
    Text.tag_remove widgets.text "selected"
      (TextIndex (LineChar (1,0), [])) (TextIndex (End, []));
    Hashtbl.clear selected

  method select i =
    if self#is_selected i then ()
    else begin
      Hashtbl.add selected i ();
      (* selection is available at most one viewer. *)
      (* why ? because it is difficult. *)
      List.iter (fun vw ->
	if vw != self#coerce then vw#reset_select) !viewers
    end

  method deselect i =
    Hashtbl.remove selected i (* no error check *)

  method highlight item onoff =
    try
      let line = self#line_of_item item in
      (if onoff then Text.tag_add else Text.tag_remove)
  	widgets.text "selected"
  	(TextIndex (LineChar (line,0), []))
  	(TextIndex (LineChar (line+1,0), []))
    with
      Failure _ -> () (* invisible *)

  method is_selected item = Hashtbl.mem selected item

  (************************************************************** open/close *)
  val opened_directory = (Hashtbl.create 107 : (dir, unit) Hashtbl.t)

  method is_opened dir =
    try Hashtbl.find opened_directory dir; true with _ -> false

  method open_dir dir =
    if self#is_opened dir then () else Hashtbl.add opened_directory dir ()

  method close_dir dir =
    Hashtbl.remove opened_directory dir

  (************************************************************** the others *)
  val indent_length = 4

  val mutable drag_possible = ref true

  method visible_descendant (d : dir) =
    let cntr = ref 0 in
    if not (self#is_opened d) then 0
    else begin
      List.iter (fun i' ->
	match i'#itemtype with
	  Dir when self#is_opened i'#dir ->
	    cntr := !cntr + self#visible_descendant i'#dir + 1
	| _ -> incr cntr) d#items;
      !cntr
    end

  method depth_of_item i =
    let rec sub i =
      if i == (topdir :> item) then 0
      else begin
	match i#parent with
	  Some p -> sub (p :> item) + 1
	| None -> raise (Failure "depth_of_item")
      end
    in
    sub i

  method line_of_item i =
    let c = ref 1 in
    let rec sub ci =
      if ci == i then raise (FoundAt !c)
      else if ci#itemtype = Dir then begin
	incr c;
	if self#is_opened ci#dir then
	  List.iter (fun cci -> sub cci) ci#dir#items
      end else
	incr c
    in
    try
      sub (topdir :> item); raise Not_found
    with
      FoundAt l -> l

  method line_of_item i =
    let rec sub i =
      if i == (topdir :> item) then 1
      else begin
	match i#parent with
	  None -> raise (Failure "line_of_item")
	| Some p ->
	    begin try
	      if not (self#is_opened p) then
		raise (Failure "line_of_item : invisible")
	      else begin
		let cntr = ref 1 in
		List.iter (function
		    i' when i== i' -> raise (FoundAt (sub (p :> item) + !cntr))
		  | i' when i'#itemtype = Dir ->
		      cntr := !cntr + self#visible_descendant i'#dir + 1
		  | _ -> incr cntr) p#items;
		raise (Failure "line_of_item : conflict in data structure")
	      end
	    with
	      FoundAt l -> l
	    end
      end
    in
    sub i

  method item_of_line l =
    let rec sub item = function
	1 -> raise (FoundItem item)
      | x ->
	  begin match item#itemtype with
	    Dir when self#is_opened item#dir ->
	      List.fold_left (fun rest i -> sub i rest) (x - 1) item#dir#items
	  | _ -> x - 1
	  end
    in
    try ignore (sub (topdir :> item) l); raise Not_found with FoundItem i -> i

  method insert_item item =
    (* if the item is the top dir, then change the dirmenub also *)
    if item == (topdir :> item) then
      Menubutton.configure widgets.dirmenub [Text item#dir#title];

    (* draw item which is logically placed but not drawn yet *)
    let line = self#line_of_item item in
(* IF WE USE THIS, WE HAVE TO CHANGE redraw_item ALSO !!!
    let the_last_line =
      match Text.index widgets.text (TextIndex (End, [CharOffset (-1)])) with
      	LineChar (1,0) -> 0
      |	LineChar (l,_) -> l
      |	_ -> raise (Failure "Text.index error (for End)")
    in
    (* if we are tring to draw over the last line, put a newline first *)
    (* except when the text is empty *)
    if the_last_line < line && the_last_line <> 0 then begin
      (* correctly, the_last_line +1 = lineopt *)
      Log.debug "Insert at last";
      Text.configure widgets.text [ State Normal ];
      Text.insert widgets.text (TextIndex (End, [])) "\n" ["normal"];
      Text.configure widgets.text [ State Disabled ]
    end;
    (* also we have to erase the last newline after all the drawing *)
    let inserted_at_the_last = the_last_line < line in
*)
    let indent = self#depth_of_item item in
    let rec sub indent item =
      let tagname = self#get_tag indent in
      let insert_indent () =
	let indent_string = String.make (indent * indent_length) ' ' in
	Text.insert widgets.text (TextIndex (LineChar (line,0), []))
	  indent_string ["normal"];
	for i = 0 to indent - 1 do
	  Text.tag_add widgets.text (self#get_tag (i+1))
	    (TextIndex (LineChar (line,indent_length*i), []))
	    (TextIndex (LineChar (line,indent_length*(i+1)), []))
	done
      in
      match item#itemtype with
  	Separator ->
  	  Text.insert widgets.text (TextIndex (LineChar (line,0), []))
  	    (String.make 30 '-' ^ "\n") [tagname];
	  insert_indent ()
      | Page ->
  	  Text.insert widgets.text (TextIndex (LineChar (line,0), []))
  	    (item#page#title ^ "\n") [tagname];
	  insert_indent ()
      | Dir ->
  	  (* opposite order *)
  	  if self#is_opened item#dir then
  	    List.iter (sub (indent + 1)) (List.rev item#dir#items);
(*
	  Text.insert widgets.text (TextIndex (LineChar (line,0), []))
	    (indent_string ^
	     (match tree#wheretoadd with
	        Some d when d == item#dir -> "@"
	      | _ -> "*") ^
	     item#dir#title ^ "\n") [tagname; "dirbutton"]
*)
	  Text.insert widgets.text (TextIndex (LineChar (line,0), []))
	    (" "^item#dir#title ^ "\n") [tagname; "dirline"];
	  Text.insert widgets.text (TextIndex (LineChar (line,0), []))
	     (match tree#wheretoadd with
	        Some d when d == item#dir -> "@"
	      | _ -> "*") [tagname; "dirbutton"];
	  insert_indent ()
    in
    Text.configure widgets.text [ State Normal ];
    sub indent item;
(*
    if inserted_at_the_last then
      (* Delete the last line *)
      Text.delete widgets.text (TextIndex (End, [CharOffset (-1)]))
                                 (TextIndex (End, []));
*)
    Text.configure widgets.text [ State Disabled ]

  method erase_item item =
    (* erase item from the text which is logically placed and drawn *)
    let line = self#line_of_item item
    in
    Text.configure widgets.text [ State Normal ];
    begin match item#itemtype with
      Dir when self#is_opened item#dir ->
	Text.delete widgets.text (TextIndex (LineChar (line,0), []))
	  (TextIndex (LineChar (line + self#visible_descendant item#dir + 1,0), []))
    | _ ->
	Text.delete widgets.text (TextIndex (LineChar (line,0), []))
	  (TextIndex (LineChar (line+1,0), []))
    end;
    Text.configure widgets.text [ State Disabled ]

  method redraw_item item =
    (* redraw item whose information is changed, but at the same place *)
(*
    (* This is quite slow for the opened directories *)
    self#erase_item item;
    self#insert_item item
*)
    (* erase item from the text which is logically placed and drawn *)
    let line = self#line_of_item item
    in
    Text.configure widgets.text [ State Normal ];
    (* erase: for opened directories, the contents are kept *)
    Text.delete widgets.text (TextIndex (LineChar (line,0), []))
	(TextIndex (LineChar (line+1,0), []));

    (* insert again *)

    (* if the item is the top dir, then change the dirmenub also *)
    if item == (topdir :> item) then
      Menubutton.configure widgets.dirmenub [Text item#dir#title];

    let indent = self#depth_of_item item in
    let rec sub indent item =
      let tagname = self#get_tag indent in
      let insert_indent () =
	let indent_string = String.make (indent * indent_length) ' ' in
	Text.insert widgets.text (TextIndex (LineChar (line,0), []))
	  indent_string ["normal"];
	for i = 0 to indent - 1 do
	  Text.tag_add widgets.text (self#get_tag (i+1))
	    (TextIndex (LineChar (line,indent_length*i), []))
	    (TextIndex (LineChar (line,indent_length*(i+1)), []))
	done
      in
      match item#itemtype with
  	Separator ->
  	  Text.insert widgets.text (TextIndex (LineChar (line,0), []))
  	    (String.make 30 '-' ^ "\n") [tagname];
	  insert_indent ()
      | Page ->
  	  Text.insert widgets.text (TextIndex (LineChar (line,0), []))
  	    (item#page#title ^ "\n") [tagname];
	  insert_indent ()
      | Dir ->
          (* NO DRAWING FOR CHILDREN BECAUSE THEY ARE THERE *)
	  Text.insert widgets.text (TextIndex (LineChar (line,0), []))
	    (" "^item#dir#title ^ "\n") [tagname; "dirline"];
	  Text.insert widgets.text (TextIndex (LineChar (line,0), []))
	     (match tree#wheretoadd with
	        Some d when d == item#dir -> "@"
	      | _ -> "*") [tagname; "dirbutton"];
	  insert_indent ()
    in
    sub indent item;
    Text.configure widgets.text [ State Disabled ]


  method display (tr : Tree.tree) (dir : Tree.dir) =
    tree <- tr;
    topdir <- dir;
    Hashtbl.clear selected;
    (* opened directory setting (not ideal) *)
    Hashtbl.clear opened_directory;
    tree#root#iter (
      ( fun d -> if d#status = Opened then self#open_dir d ),
      ( fun _ -> () ), (fun _ -> ()) );
    self#open_dir dir;

    (* menu *)
    Menubutton.configure widgets.dirmenub [Text dir#title];
    Menu.delete widgets.dirmenu (Number 0) Last;
    let rec add_menu_item = function
	Some p ->
	  Menu.add_command widgets.dirmenu [
	    Label p#title;
	    Command (fun () -> self#display self#tree p) ];
	  add_menu_item p#parent
      |	None -> ()
    in
    add_menu_item dir#parent;

    (* clear *)
    Text.configure widgets.text [ State Normal ];
    Text.delete widgets.text (TextIndex (LineChar (1,0), []))
      (TextIndex (End, []));
    Text.configure widgets.text [ State Disabled ];
    self#insert_item (dir :> item)

  method check_sanity dest =
    if self#is_selected dest then false
    else begin
      if dest == (topdir :> item) then true
      else begin
	match dest#parent with
	  Some p -> self#check_sanity (p :> item)
	| None -> raise (Failure "check_sanity overrun")
      end
    end

  method is_visible (item : item) =
    if item == (topdir :> item) then true
    else begin
      match item#parent with
	Some p ->
	  if not (self#is_opened p) then false else self#is_visible (p :> item)
      |	None -> false
    end
	
  method popup (item : item) =
    match item#itemtype with
      Page ->
	[ [Label "Edit"; Command (fun () -> edit item)];
	  [Label "Goto"; Command (fun () ->
	    Connection.client_navigator item#page#url)];
	  [Label "Add new directory here"; Command (fun () ->
	    let p = match item#parent with
	      Some p -> p
	    | None -> raise (Failure "insert_new_directory impossible")
	    in
	    let dir = new dir () in
	    dir#set_title "New directory";
	    let rec sub = function
		[] -> raise (Failure "insert-new-directory")
	      |	x::xs when x == item ->
		  x::(dir :> item)::xs
	      |	x::xs -> x :: sub xs
	    in
	    p#set_items (sub p#items);
	    (* we must recompute the place we Mark "insert" *)
	    insert_for_all_viewers (dir :> item))]]
    | Dir ->
	[ [Label "Edit"; Command (fun () -> edit item)];
	  [Label "Set as Incoming Folder"; Command (fun () ->
	    let oldopt = self#tree#wheretoadd in
	    self#tree#set_wheretoadd (Some item#dir);
	    begin match oldopt with
	      Some d ->
		let i = (d :> item) in
		redraw_for_all_viewers i
	    | None -> ()
	    end) ];
	  [Label "Magnify"; Command (fun () ->
	    self#display self#tree item#dir)] ]
    | _ -> []

  method init =
    viewers := self#coerce :: !viewers;

    bind widgets.text [[], Destroy] (BindSet ([], (fun _ ->
      viewers :=
        List.fold_right (fun x st ->
	  if x == self#coerce then st else x :: st) !viewers [];
      destroy top;
      (* termination of program *)
      (* warn : if there is other toplevels (they are not viewer) *)
      (* this code kill them!!! *)
      if !viewers = [] then destroy Widget.default_toplevel)));

    Menu.add_command widgets.filemenu [
      Label "Close";
      Command (fun () -> self#close)];

    Menu.add_command widgets.filemenu [
      Label "Open new treeviewer";
      Command (fun () -> create (new treeview) self#tree) ];

    Menu.add_command widgets.filemenu [
      Label "Save";
      (* Currently only for ~/.mmm/surfboard.html *)
      Command (fun () ->
	let file = Misc.user_file ".mmm/surfboard.html" in
	Log.debug ("Writing to "^file^" ...");
	let oc = open_out file in
	self#tree#output oc;
	close_out oc;
	Log.debug "done.")];

    bindtags widgets.text [ WidgetBindings widgets.text;
			    WidgetBindings (Winfo.parent widgets.text);
			    TagBindings "all"  ];

    let mouse_x = ref 0
    and mouse_y = ref 0
    and moved = ref false
    in
    bind widgets.text [[], ButtonPressDetail 2]
      (BindSet ([Ev_MouseX; Ev_MouseY], fun ev ->
	Text.scan_mark widgets.text ev.ev_MouseX ev.ev_MouseY;
	mouse_x := ev.ev_MouseX;
	mouse_y := ev.ev_MouseY;
	moved := false));
    bind widgets.text [[Button2], Motion]
      (BindSet ([Ev_MouseX; Ev_MouseY], fun ev ->
	if !mouse_x <> ev.ev_MouseX || !mouse_y <> ev.ev_MouseY then
	  moved := true;
	if !moved then
	  Text.scan_dragto widgets.text ev.ev_MouseX ev.ev_MouseY));

    (* debug *)
    bind widgets.text [[], ButtonPressDetail 1] (BindSet ([], (fun _ ->
      match Text.index widgets.text (TextIndex (Mark "current", [])) with
	LineChar (l,_) ->
	  let i = self#item_of_line l in
	  begin match i#itemtype with
	    Separator -> Log.f "---"
	  | Page -> Log.f i#page#title
	  | Dir -> Log.f i#dir#title
	  end
      | _ -> raise (Failure "Text.index error"))));

    let drag_start = ref (tree#root :> item) (* any value *)
    in

    (* DD Start *)
    bind widgets.text [[], ButtonPressDetail 1] (BindExtend ([], (fun _ ->
      drag_possible := true;
      Grab.set widgets.text;
      Text.configure widgets.text [Cursor (XCursor "plus")];
      let l, item =
	match Text.index widgets.text (TextIndex (Mark "current", [])) with
	  LineChar (l,_) -> l, self#item_of_line l
	| _ -> raise (Failure "Text.index error")
      in
      drag_start := item)));

    (* DD activate *)
    bind widgets.text [[], ButtonReleaseDetail 1] (BindSet (
	   [Ev_RootX; Ev_RootY], (fun ev ->
try
      Text.configure widgets.text [Cursor (XCursor "top_left_arrow")];
      Grab.release widgets.text;

      let cw =
	try
	  Winfo.containing ev.ev_RootX ev.ev_RootY
	with
	  _ -> raise (Failure "no context widget")
      in
      let vw =
        let rec find =
          function
            | [] ->
  	        Bell.ring ();
  	        raise (Failure "out of text widgets")
            | vw :: viewers ->
  	        if vw#is_my_text cw then vw else find viewers
        in find !viewers
      in

      let (*l,*) dest, dropin =
	match update (); Text.index cw (TextIndex (Mark "current", [])) with
	  LineChar (l,c) ->
	    Log.debug (Printf.sprintf "%d,%d" l c);
	    let item = vw#item_of_line l in
	    let d = vw#depth_of_item item in
	    if d * indent_length = c then (*l,*) item, true
	    else (*l,*) item, false
	| _ -> raise (Failure "Text.index error")
      in
      if !drag_start != dest && !drag_possible then begin (* dropped !! *)
Log.debug (Printf.sprintf "DD!! %s -> %s"
(!drag_start#name)
(dest#name));
	let is =
	  let buf = ref [] in
	  Hashtbl.iter (fun k () ->
Log.debug k#name;
	    buf := k :: !buf) selected;
	  !buf
	in
	let is = if is <> [] then is else [!drag_start] in
Log.debug (Printf.sprintf "Selected %d" (List.length is));
	(* remove all selection --- it is not useful but it is easy *)
	self#reset_select;
	if not (self#check_sanity dest) ||
	   (dest#parent = None && not dropin) then Bell.ring ()
	else begin
	    (* remove *)
	    List.iter (fun i ->
	      erase_for_all_viewers i;
  	      i#remove_self ()) is;
  	    if dest#itemtype = Dir && dropin then begin
  		(* Log.debug "drop-in"; *)
  		dest#dir#set_items (is @ dest#dir#items);
	        List.iter (fun i -> insert_for_all_viewers i) is
  	    end else begin
  	      (* Log.debug "insert-after"; *)
	      let p = match dest#parent with
		Some p -> p
	      |	None -> raise (Failure "insert-after impossible")
	      in
Log.debug ("Insert after "^dest#name ^ " @ " ^ p#name);
	      let rec sub = function
		  [] -> raise (Failure "insert-after")
		| x::xs when x == dest ->
		    x::(is@xs)
		| x::xs -> x :: sub xs
	      in
	      p#set_items (sub p#items);
	      (* we must recompute the place we Mark "insert" *)
	      List.iter (fun i  -> insert_for_all_viewers i) is
	    end
	  end
	end
with Failure s -> Log.f s
 )));

    (* popup *)
    let menu = Menu.create widgets.text [TearOff false] in
    bind widgets.text [[], ButtonPressDetail 3]
    (BindSet ([Ev_RootX; Ev_RootY], (fun ev ->
      let l, item =
	match Text.index widgets.text (TextIndex (Mark "current", [])) with
	  LineChar (l,_) ->
	    let item = self#item_of_line l in
	    l, item
	| _ -> raise (Failure "Text.index error")
      in
      let menulist = self#popup item in
      Menu.delete menu (Number 0) End;
      List.iter (fun opts ->
	Menu.add_command menu opts) menulist;
      Menu.popup menu ev.ev_RootX ev.ev_RootY )));

    (* selection *)
    List.iter (fun ev ->
      bind widgets.text ev (BindExtend ([], (fun _ ->
	let l, item =
	  match Text.index widgets.text (TextIndex (Mark "current", [])) with
	    LineChar (l,_) -> l, self#item_of_line l
	  | _ -> raise (Failure "Text.index error")
	in
	if not (self#is_selected item) then begin
	  Hashtbl.iter (fun k () ->
	    self#deselect k;
	    self#highlight k false) selected;
	  self#select item;
	  self#highlight item true
	end))))
    [ [[], ButtonPressDetail 1]; [[], ButtonPressDetail 3] ] ;

    bind widgets.text [[Shift], ButtonPressDetail 1] (BindSet ([], (fun _ ->
      drag_possible := false;
      let l, item =
	match Text.index widgets.text (TextIndex (Mark "current", [])) with
	  LineChar (l,_) -> l, self#item_of_line l
	| _ -> raise (Failure "Text.index error")
      in
      if self#is_selected item then begin
	self#deselect item ;
	self#highlight item false
      end else begin
	self#select item;
	self#highlight item true;
      end)));

    (* activation *)
    bind widgets.text [[Double], ButtonPressDetail 1] (BindSet ([], (fun _ ->
      (* disable DD *)
      drag_possible := false;
      Grab.release widgets.text;
      Text.configure widgets.text [Cursor (XCursor "top_left_arrow")];
      let l, item =
	match Text.index widgets.text (TextIndex (Mark "current", [])) with
	  LineChar (l,_) ->
	    let item = self#item_of_line l in
	    l, item
	| _ -> raise (Failure "Text.index error")
      in
      (* erase selection (just for debug for dir) *)
      self#deselect item;
      self#highlight item false;
      let l = self#line_of_item item in
      begin match item#itemtype with
	Dir ->
	  if self#is_opened item#dir then begin
	    (* Log.debug ("Close " ^ (name item)); *)
	    let vds = self#visible_descendant item#dir in
	
	    Text.configure widgets.text [ State Normal ];
	    Text.delete widgets.text (TextIndex (LineChar (l+1,0), []))
	      (TextIndex (LineChar (l+vds+1,0), []));
	    Text.configure widgets.text [ State Disabled ];
	    item#dir#iter ((fun dps -> self#deselect (dps :> item)),
			   (fun dps -> self#deselect (dps :> item)),
			   (fun dps -> self#deselect (dps :> item)));
	    self#close_dir item#dir
	  end else begin
	    (* Log.debug ("Open " ^ (name item)); *)
	    self#open_dir item#dir;
	    List.iter (fun i ->
	      self#insert_item i;
	      (* dirty tk... *)
	      self#deselect i) item#dir#items
	  end
      | Page -> Connection.client_navigator item#page#url
      | Separator -> ()
      end)))

  method close =
    (* just destroy myself. it is removed from viewers (see init) *)
    destroy widgets.text

  method is_my_text w = (widgets.text = w)
end;;

let create = create (new treeview);;




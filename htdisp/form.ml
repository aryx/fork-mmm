(* Tk based FormDisplay  *)
open Printf
open Tk
open Hyper
open Www
open Html
open Htmlfmt
open Maps
open Embed
open Viewers

let form_bg = ref "#d9d9d9"

type t = {
  text_input : Widget.widget -> tag -> unit;
      (* [text_input top tag] *)
  checkbox_input : Widget.widget ->  tag -> unit;
      (* [input top tag] *)
  radio_input : Widget.widget ->  tag -> unit;
      (* [input top tag] *)
  image_input : Widget.widget ->  tag -> embobject;
      (* [input top tag] *)
  submit_input : Widget.widget ->  tag -> unit;
      (* [input top tag] *)
  reset_input : Widget.widget ->  tag -> unit;
      (* [input top tag] *)
  select : Widget.widget -> (string * string * bool) list -> tag -> unit;
      (* [select top elements tag] *)
  textarea:  Widget.widget -> string -> tag -> unit
      (* [textarea top initial attrs] *)
  }

(* Most of the widgets created here are created with [TakeFocus false];
 * The reason is that otherwise, in "focus follows mouse" mode, when scrolling
 * a text widget containing form elements, the mouse may come over one of
 * these elements; it would then set the focus to the element, and break
 * further scrolling. Instead, with [TakeFocus false], the user has to
 * click explicitly in the widgets (especially entries and text) in order
 * to fill them.
 * However, we do attempt to re-implement the Tab/Shift-Tab system that we
 * inevitably broke by setting [TakeFocus false].
 * Phew.
 *)


(* mapi (fun n x -> ...) n0 l *)
let rec mapi f n l =
  match l with
    [] -> [] 
  | x::l -> let v = f n x in v::(mapi f (succ n) l)

(* Do our own Tab/Shift-Tab : don't call it for Text widgets ! *)
let focus_link prev w =
  match !prev with
    None -> prev := Some w;
  | Some old ->
      bind old [[], KeyPressDetail "Tab"]
       (BindSetBreakable ([], fun ei -> try Focus.set w with _ -> ()));
      bind w [[Shift], KeyPressDetail "Tab"]
       (BindSetBreakable ([], fun ei -> try Focus.set old with _ -> ()));
      prev := Some w

(* A TEXT or PASSWORD input *)
(* TODO: MAXLENGTH *)
let text_input prev_widget ctx behav top tag =
  try
    let name = get_attribute tag "name"
    and inputtype = get_attribute tag "type" in
    (* Create an entry widget : don't take focus unless we click in it *)
    let e = Entry.create top [ExportSelection false; TakeFocus false; 
			      Background (NamedColor !form_bg)] in
    (* Check for size *)
    begin try 
     let s = get_attribute tag "size" in
      try Entry.configure e [TextWidth (int_of_string s)]
      with Failure "int_of_string" ->
	 Log.f (sprintf "%s not a valid val for SIZE" s)
    with Not_found -> ()
    end;
    (* Check for passwd *)
    if String.lowercase inputtype = "password" then 
      Entry.configure e [Show '*'];
    (* The behaviours *)
    let reset = 
      try
	let v = get_attribute tag "value" in
	 Entry.insert e End v;
	 (fun () -> Entry.delete_range e (Number 0) End;
		    Entry.insert e End v)
      with Not_found ->
	 (fun () -> Entry.delete_range e (Number 0) End)
    (* spec says we could omit if empty *)
    and get_value () = [name, Entry.get e]
    in
      focus_link prev_widget e;
      pack [e][];
      behav#add_get EntryInput get_value;
      behav#add_reset reset;
     (* single-entry : enter submits the form *)
      Tk.bind e [[], KeyPressDetail "Return"] 
         (BindSet ([], (fun _ -> 
	   match behav#single_submit with
	     Some h -> ctx#goto h
	   | None -> ())))
  with
    Not_found ->
      raise (Invalid_Html "Missing NAME in TEXT or PASSWORD")


(* A CHECKBOX input *)
let checkbox_input prev_widget behav top tag =
  try
    let name = get_attribute tag "name" in
    let v = Textvariable.create_temporary top in  (* variable val is 1/0 *)
    let c = Checkbutton.create top [Variable v; TakeFocus false] in
    let reset =
      if has_attribute tag "checked" then begin
	Checkbutton.select c;
	(fun () -> Checkbutton.select c)
	end
      else (fun () -> Checkbutton.deselect c)
    and get_value =
      let value = 
	try get_attribute tag "value" 
	with Not_found ->
         (* other browsers seem to use "on". Thanks to Dave Love for pointing
            this out *)
	 Log.f "no VALUE given for input CHECKBOX, using \"on\"";
	 "on" in
      (* spec says we SHOULD omit when not selected *)
      (fun () -> 
	 match Textvariable.get v with 
	   "1" -> [name, value]
	 | _ -> [])
    in
      focus_link prev_widget c;
      pack [c][];
      behav#add_get OtherInput get_value;
      behav#add_reset reset
  with
    Not_found ->
      raise (Invalid_Html "Missing NAME in CHECKBOX")


(* A RADIO input *)
(* ONLY THE FIRST BUTTON RESET/GET_VALUE IS USED *)
let radio_input prev_widget behav = 
  (* Table of radio names *)
  let radios = Hashtbl.create 17 in
  (fun top tag ->
     try
       let name = get_attribute tag "name" in
       let r = Radiobutton.create top [TakeFocus false]
       and checked = has_attribute tag "checked"
       and va = try get_attribute tag "value" with Not_found -> "on"
       in
       try
	 let v, sel = Hashtbl.find radios name in
	   (* We already have a radiobutton with this name *)
	   Radiobutton.configure r [Variable v; Value va];
	   if checked then begin 
	     Radiobutton.select r; (* select it *)
	     sel := r (* store it in table for reset *)
	   end;
	   (* no need to add behaviour *)
	   focus_link prev_widget r;
	   pack [r][]
       with
	 Not_found ->
	   (* this is the first radio button with this name *)
	   (* it this thus assumed checked *)
	    let v = Textvariable.create_temporary top in
	     Hashtbl.add radios name (v, ref r);
	    let get_value () = [name, Textvariable.get v]
	    and reset () = 
	       (* to reset, we must lookup the table *)
	      let _, sel = Hashtbl.find radios name in
		Radiobutton.select !sel 
	    in
	    Radiobutton.configure r [Variable v; Value va];
	    Radiobutton.select r; (* assume selected *)
	    focus_link prev_widget r;
	    pack [r][];
	    behav#add_get OtherInput get_value;
	    behav#add_reset reset
    with
      Not_found ->
	raise (Invalid_Html "Missing NAME in RADIO"))


(* An IMAGE input 
 * Q: no target here ?
 *)
let image_input prev_widget ctx base behav top tag =
  try
    let n = get_attribute tag "name" in
    let src = get_attribute tag "src" in
    let alt = 
       try get_attribute tag "alt"
       with Not_found -> "[INPUT IMAGE]"
    in
      {
      embed_hlink = 
          { h_uri = src; h_context = Some base; h_method = GET; h_params =[] };
      embed_frame = top;
      embed_context = ctx; (* pass as is... *)
      embed_map = FormMap (fun (x, y) ->
			     let subargs =
                                [sprintf "%s.x" n, string_of_int x;
                                 sprintf "%s.y" n, string_of_int y] in
			       behav#submit subargs);
      embed_alt = alt}
  with
  Not_found ->
    raise (Invalid_Html "missing NAME or SRC in input IMAGE")


(* A Submit button *)
let submit_input prev_widget ctx behav top tag = 
  let l = 
    try get_attribute tag "value"
    with Not_found -> I18n.sprintf "Submit"
  in
  try
    let n = get_attribute tag "name" in
    pack [Button.create top [Text l; TakeFocus false;
	     Command (fun () -> ctx#goto (behav#submit [n,l]))]]
         []
  with
    Not_found ->
     (* if name is not present, the button does not contribute a value *)
     pack [Button.create top [Text l; TakeFocus false;
	      Command (fun () -> ctx#goto (behav#submit []))]]
	  []


let reset_input prev_widget behav top tag = 
  let l = 
    try get_attribute tag "value"
    with Not_found -> I18n.sprintf "Reset" in
  let b = Button.create top [Text l; TakeFocus false;
			     Command (fun () -> behav#reset)] in
    pack[b][]



 (* TODO: FILE (RFC 1867) *)



(* A SELECT list *)
(* options is: (val, displayed thing, selected) list *)    
let select prev_widget behav top options tag =
  let name = get_attribute tag "name" in
  let ssize = get_attribute tag "size" in
  let size =
     try int_of_string ssize
     with _ -> 
	Log.f (sprintf "%s not a valid val for SIZE" ssize);
	5 in
  let multiple = has_attribute tag "multiple" in
  (* assume 20 vertical pixels per menu item *)
  let fit_vertical n = 20 * n < Winfo.screenheight top in
  if size = 1 && not multiple && fit_vertical (List.length options)
  then begin (* menus larger than screen are bad, use an optionmenu *)
    let vard = Textvariable.create_temporary top   (* var to display *)
    and varv = Textvariable.create_temporary top   (* var for val *)
    in
    let m = Menubutton.create top 
         [TextVariable vard; Relief Raised; Anchor Center; TakeFocus false] in
    let mmenu = Menu.create m [TearOff false] in
     Menubutton.configure m [Menu mmenu];
     let initial =
       match options with
	 [] -> raise (Invalid_Html ("No OPTION in SELECT"))
       | opt :: _ -> ref opt in
     List.iter (function (v,d,s) as x ->
		Menu.add_command mmenu
		    [Label d;
		     Command (fun () -> 
			       Textvariable.set varv v;
			       Textvariable.set vard d
			      )];
		if s then initial := x
		)
	      options;
     let reset () =
       match !initial with
	(v,d,_) -> 
	     Textvariable.set varv v;
	     Textvariable.set vard d
     and get_value () = [name, Textvariable.get varv] in
       reset();
       focus_link prev_widget m;
       pack [m][];
       behav#add_get OtherInput get_value;
       behav#add_reset reset
     end
  else begin (* use a listbox *)
   (* listbox indices start at 0 *)
   (* we must not ExportSelection, otherwise one unique listbox can *)
   (* have a current selection *)
    let nth_entry n l =
      let (v,_,_) = List.nth l n in v in
    let f,lb = Frx_listbox.new_scrollable_listbox top 
      [TextHeight size; TextWidth 0; (* automatic size *)
       (if multiple then SelectMode Multiple else SelectMode Single);
       ExportSelection false; Background (NamedColor !form_bg)] in
      Listbox.configure lb [TakeFocus false];
    let initial = ref [] in
    let entries =
     mapi (fun i (_,v,s) -> 
	       if s then initial := i :: !initial;
	       v) 0 options in
      Listbox.insert lb End entries;
    if !initial = [] then initial := [0];
    let reset () = 
       Listbox.selection_clear lb (Number 0) End;
       List.iter (fun i ->
		 Listbox.selection_set lb (Number i)(Number i))
	       !initial
    and get_value () =
       List.map (function
	      Number n -> name, nth_entry n options
	    | _ -> name, nth_entry 0 options (* fatal error ! *))
	   (Listbox.curselection lb)
    in
      reset (); 
      focus_link prev_widget f;
      pack [f][];
      behav#add_reset reset;
      behav#add_get OtherInput get_value
    end

let textarea prev_widget behav top initial tag = 
  try 
    let name = get_attribute tag "name" in
    let f,t = 
       Frx_text.new_scrollable_text top
           [ExportSelection false; TakeFocus false] false in
    Text.configure t [Background (NamedColor !form_bg)];
    begin try
      let w = get_attribute tag "cols" in
      try Text.configure t [TextWidth (int_of_string w)]
      with Failure "int_of_string" ->
	Log.f (sprintf "%s not a valid val for COLS" w)
    with Not_found -> ()
    end;
    begin try
      let h = get_attribute tag "rows" in
      try Text.configure t [TextHeight (int_of_string h)]
      with Failure "int_of_string" ->
	Log.f (sprintf "%s not a valid val for ROWS" h)
    with Not_found -> ()
    end;
    Text.insert t Frx_text.textEnd initial [];
    let reset () =
      Text.delete t (TextIndex(LineChar(0,0), [])) Frx_text.textEnd;
      Text.insert t Frx_text.textEnd initial []
    and get_value () =
      [name, Text.get t (TextIndex(LineChar(0,0), [])) 
		   (TextIndex(End, [CharOffset (-1)]))]
    in
       pack [f][];
       behav#add_reset reset;
       behav#add_get EntryInput get_value
  with
    Not_found -> raise (Invalid_Html "Missing NAME in TEXTAREA")


let create base behav ctx =
  let prev_widget = ref None in
  { text_input = text_input prev_widget ctx behav;
    checkbox_input = checkbox_input prev_widget behav;
    radio_input = radio_input prev_widget behav;
    image_input = image_input prev_widget ctx base behav;
    submit_input = submit_input prev_widget ctx behav;
    reset_input = reset_input prev_widget behav;
    select = select prev_widget behav;
    textarea = textarea prev_widget behav
  }

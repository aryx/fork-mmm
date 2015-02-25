open Printf
open Tk

(* some resource utilities : cache and parser for comme types *)

(* Resource.get returns "" when not found *)
let get name = 
  Resource.get Widget.default_toplevel name name

(* Getting resource from X Server should be heavy operation. *)
(* Use cache instead ... *)
let resource_table = Hashtbl.create 107
let get name =
  try 
    Hashtbl.find resource_table name 
  with 
    Not_found ->
      let r = Resource.get Widget.default_toplevel name name in
      Hashtbl.add resource_table name r;
      r

let readfile f level = 
  Hashtbl.clear resource_table;
  Resource.readfile f level

(*
 * String resources
 *)

let string name default =
  match get name with
    "" -> default
  | s -> s

(*
 * Comma separated lists of strings
 *)
let stringlist name default =
  match get name with
    "" -> default
  | s -> 
      (* remove the head and tail spaces and tabs *)
      let s = Str.replace_first (Str.regexp "^[ \t]*") "" s in
      let s = Str.replace_first (Str.regexp "[ \t]*$") "" s in
      match Str.split (Str.regexp "[ \t]*,[ \t]*") s with
	[] -> default
      |	l -> l

(*
 * Integers
 *)
let int name default =
  try
    int_of_string (get name)
  with
    Failure "int_of_string" -> default


(*
 * Relief
 *)
let relief name default =
  match get name with
    "raised" -> Raised
  | "sunken" -> Sunken
  | "flat" -> Flat
  | "ridge" -> Ridge
  | "groove" -> Groove
  | _ -> default


(* 
 * Events
 *  we provide a parser for event descriptions in Tcl/Tk-syntax
 *  (this should actually be in camltk)
 *)

(* the lexer *)
type token = 
  | StartEvent
  | Ident of string
  | Int of int
  | EndEvent

let rec ident ebuf = parser
  | [< '  'A'..'Z'|'a'..'z'|'0'..'9' as c; s >] ->
      Ebuffer.output_char ebuf c; ident ebuf s
  | [<>] -> Ident (Ebuffer.get ebuf)

let rec num ebuf = parser 
  | [< '  '0'..'9' as c; s >] ->
      Ebuffer.output_char ebuf c; num ebuf s
  | [<>] -> Int (int_of_string (Ebuffer.get ebuf))

let rec token = parser
  | [< '  '<' >] -> StartEvent
  | [< '  '>' >] -> EndEvent
  | [< '  'A'..'Z'|'a'..'z' as c; s >] ->
      let e = Ebuffer.create 16 in
      Ebuffer.output_char e c;
      ident e s
  | [< '  '0'..'9' as c; s >] ->
      let e = Ebuffer.create 16 in
      Ebuffer.output_char e c;
      num e s
  | [< '  '-'; s>] -> token s
  (* also allow spaces as separators *)
  | [< '  ' '|'\t'; s>] -> token s

(* the parser *)
let modifier = parser
  | [< 'Ident "Control" >] -> Control
  | [< 'Ident "Shift" >] -> Shift
  | [< 'Ident "Lock" >] -> Lock
  | [< 'Ident "Button1" >] -> Button1 | [< 'Ident "B1" >] -> Button1
  | [< 'Ident "Button2" >] -> Button2 | [< 'Ident "B2" >] -> Button2
  | [< 'Ident "Button3" >] -> Button3 | [< 'Ident "B3" >] -> Button3
  | [< 'Ident "Button4" >] -> Button4 | [< 'Ident "B4" >] -> Button4
  | [< 'Ident "Button5" >] -> Button5 | [< 'Ident "B5" >] -> Button5
  | [< 'Ident "Double" >] -> Double
  | [< 'Ident "Triple" >] -> Triple
  | [< 'Ident "Mod1" >] -> Mod1 | [< 'Ident "M1" >] -> Mod1
  | [< 'Ident "Mod2" >] -> Mod2 | [< 'Ident "M2" >] -> Mod2
  | [< 'Ident "Mod3" >] -> Mod3 | [< 'Ident "M3" >] -> Mod3
  | [< 'Ident "Mod4" >] -> Mod4 | [< 'Ident "M4" >] -> Mod4
  | [< 'Ident "Mod5" >] -> Mod5 | [< 'Ident "M5" >] -> Mod5
  | [< 'Ident "Meta" >] -> Meta | [< 'Ident "M" >] -> Meta
  | [< 'Ident "Alt" >] -> Alt

let event = parser
  | [< 'Ident "ButtonPress" >] -> ButtonPress 
  | [< 'Ident "Button" >] -> ButtonPress
  | [< 'Ident "ButtonRelease" >] -> ButtonRelease
  | [< 'Ident "Circulate" >] -> Circulate
  | [< 'Ident "Colormap" >] -> ColorMap
  | [< 'Ident "Configure" >] -> Configure
  | [< 'Ident "Destroy" >] -> Destroy
  | [< 'Ident "Enter" >] -> Enter
  | [< 'Ident "Expose" >] -> Expose
  | [< 'Ident "FocusIn" >] -> FocusIn
  | [< 'Ident "FocusOut" >] -> FocusOut
  | [< 'Ident "Gravity" >] -> Gravity
  | [< 'Ident "KeyPress" >] -> KeyPress
  | [< 'Ident "Key" >] -> KeyPress
  | [< 'Ident "KeyRelease" >] -> KeyRelease
  | [< 'Ident "Leave" >] -> Leave
  | [< 'Ident "Map" >] -> Map
  | [< 'Ident "Motion" >] -> Motion
  | [< 'Ident "Property" >] -> Property
  | [< 'Ident "Reparent" >] -> Reparent
  | [< 'Ident "Unmap" >] -> Unmap
  | [< 'Ident "Visibility" >] -> Visibility

(* An event has modifiers and an event type *)
let rec event_constituents mods = parser
  | [< m = modifier; s >] -> event_constituents (m::mods) s
  | [< e = event; s >] -> begin
      (* do extra parsing for "long events" *)
      let real_e = match e with
	ButtonPress ->
	  begin match s with parser
	  | [< 'Int n >] -> ButtonPressDetail n
	  | [<>] -> ButtonPress
	  end
      |	ButtonRelease ->
	  begin match s with parser
	  | [< 'Int n >] -> ButtonReleaseDetail n
	  | [<>] -> ButtonRelease
	  end
      |	KeyPress ->
	  begin match s with parser
	  | [< 'Ident s >] -> KeyPressDetail s
	  | [<>] -> KeyPress
	  end
      |	KeyRelease ->
	  begin match s with parser
	  | [< 'Ident s >] -> KeyReleaseDetail s
	  | [<>] -> KeyRelease
	  end
      |	_ -> e
      in
      List.rev mods, real_e
  end
  (* shortcuts : an integer is ButtonPress *)
  | [< 'Int n >] -> List.rev mods, ButtonPressDetail n
  (* assume all other idents to be keysyms *)
  | [< 'Ident s >] -> List.rev mods, KeyPressDetail s


let event = parser
    [< 'StartEvent; e = event_constituents []; 'EndEvent >] -> e

(* standard combinator *)
let rec list_of p = parser
  | [< e = p ; l = list_of p>] -> e::l
  | [<>] -> []			 

let event_sequence = list_of event

let token_stream s = 
  Stream.from (fun n -> try Some (token s) with Stream.Failure -> None)

let parse_event s = event (token_stream s)
let parse_event_sequence s = event_sequence (token_stream s)

(* This one is exported *)
let event_sequence name default =
  let s = get name in
  if s = "" then default
  else 
    try event_sequence (token_stream (Stream.of_string s))
    with
      Stream.Failure | Stream.Error _ -> 
	Log.f (sprintf "Error parsing binding of %s" name);
	default
    

(* 
 * The pretty printer for event sequences.
 * Basically the same as Tk.cCAMLtoTKxEvent, but it creates short strings
 * more readable by humans
 *)

let cCAMLtoTKxEvent = function
    ButtonPress -> "Button"
  | ButtonPressDetail n -> string_of_int n
  | ButtonRelease -> "ButtonRelease"
  | ButtonReleaseDetail n -> "ButtonRelease-"^string_of_int n
  | Circulate -> "Circulate"
  | ColorMap -> "Colormap"
  | Configure -> "Configure"
  | Destroy -> "Destroy"
  | Enter -> "Enter"
  | Expose -> "Expose"
  | FocusIn -> "FocusIn"
  | FocusOut -> "FocusOut"
  | Gravity -> "Gravity"
  | KeyPress -> "Key"
  | KeyPressDetail s -> s
  | KeyRelease -> "KeyRelease"
  | KeyReleaseDetail s -> "KeyRelease-"^s
  | Leave -> "Leave"
  | Map -> "Map"
  | Motion -> "Motion"
  | Property -> "Property"
  | Reparent -> "Reparent"
  | Unmap -> "Unmap"
  | Visibility -> "Visibility" 


(* these one would be compatible with Tk syntax (short format) *)
let cCAMLtoTKevent (ml, xe) =
  sprintf "<%s%s>"
    (String.concat "" (List.map cCAMLtoTKmodifier ml))
    (cCAMLtoTKxEvent xe)

let pp_event_sequence l =
  String.concat " " (List.map cCAMLtoTKevent l)

(* this one is for describing events in menus (accelerators) *)
let shortevent (ml, xe) =
  sprintf "%s%s"
    (String.concat "" (List.map cCAMLtoTKmodifier ml))
    (cCAMLtoTKxEvent xe)

let short_event_sequence l =
  String.concat " " (List.map shortevent l)

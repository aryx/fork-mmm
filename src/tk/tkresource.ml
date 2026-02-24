open Printf
open Tk

(* some resource utilities : cache and parser for comme types *)

(* Resource.get returns "" when not found *)
(*
let get name = 
  Resource.get Widget.default_toplevel name name
*)

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

(* old parser using the Stream extension not available since 4.08 *)

(* CHATGPT: *)
let is_letter = function
  | 'a'..'z' | 'A'..'Z' -> true
  | _ -> false

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let lexer input =
  let len = String.length input in
  let pos = ref 0 in

  let rec skip () =
    if !pos < len then
      match input.[!pos] with
      | ' ' | '\t' | '-' ->
          incr pos; skip ()
      | _ -> ()
  in

  let next_token _ =
    skip ();
    if !pos >= len then
      None
    else
      match input.[!pos] with
      | '<' ->
          incr pos;
          Some StartEvent
      | '>' ->
          incr pos;
          Some EndEvent
      | c when is_letter c ->
          let buf = Buffer.create 16 in
          while !pos < len &&
                (is_letter input.[!pos] || is_digit input.[!pos])
          do
            Buffer.add_char buf input.[!pos];
            incr pos
          done;
          Some (Ident (Buffer.contents buf))
      | c when is_digit c ->
          let buf = Buffer.create 16 in
          while !pos < len && is_digit input.[!pos] do
            Buffer.add_char buf input.[!pos];
            incr pos
          done;
          Some (Int (int_of_string (Buffer.contents buf)))
      | c ->
          failwith ("Unexpected character: " ^ String.make 1 c)
  in

  Stream.from next_token

exception Parse_error of string

let expect stream tok =
  match Stream.peek stream with
  | Some t when t = tok -> Stream.junk stream
  | _ -> raise (Parse_error "Unexpected token")


(* OLD:
let rec ident ebuf = parser
  | [< '  'A'..'Z'|'a'..'z'|'0'..'9' as c; s >] ->
      Ebuffer.output_char ebuf c; ident ebuf s
  | [<>] -> Ident (Ebuffer.get ebuf)
*)



(* OLD:
let rec num ebuf = parser 
  | [< '  '0'..'9' as c; s >] ->
      Ebuffer.output_char ebuf c; num ebuf s
  | [<>] -> Int (int_of_string (Ebuffer.get ebuf))
*)

(* OLD:
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
*)

(* the parser *)
(* OLD:
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
*)

let parse_modifier = function
  | Ident "Control" -> Some Control
  | Ident "Shift" -> Some Shift
  | Ident "Lock" -> Some Lock
  | Ident "Button1" | Ident "B1" -> Some Button1
  | Ident "Button2" | Ident "B2" -> Some Button2
  | Ident "Button3" | Ident "B3" -> Some Button3
  | Ident "Button4" | Ident "B4" -> Some Button4
  | Ident "Button5" | Ident "B5" -> Some Button5
  | Ident "Double" -> Some Double
  | Ident "Triple" -> Some Triple
  | Ident "Mod1" | Ident "M1" -> Some Mod1
  | Ident "Mod2" | Ident "M2" -> Some Mod2
  | Ident "Mod3" | Ident "M3" -> Some Mod3
  | Ident "Mod4" | Ident "M4" -> Some Mod4
  | Ident "Mod5" | Ident "M5" -> Some Mod5
  | Ident "Meta" | Ident "M" -> Some Meta
  | Ident "Alt" -> Some Alt
  | _ -> None

(* OLD:
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
*)

let parse_event_type = function
  | Ident "ButtonPress" | Ident "Button" -> Some ButtonPress
  | Ident "ButtonRelease" -> Some ButtonRelease
  | Ident "Circulate" -> Some Circulate
  | Ident "Colormap" -> Some ColorMap
  | Ident "Configure" -> Some Configure
  | Ident "Destroy" -> Some Destroy
  | Ident "Enter" -> Some Enter
  | Ident "Expose" -> Some Expose
  | Ident "FocusIn" -> Some FocusIn
  | Ident "FocusOut" -> Some FocusOut
  | Ident "Gravity" -> Some Gravity
  | Ident "KeyPress" | Ident "Key" -> Some KeyPress
  | Ident "KeyRelease" -> Some KeyRelease
  | Ident "Leave" -> Some Leave
  | Ident "Map" -> Some Map
  | Ident "Motion" -> Some Motion
  | Ident "Property" -> Some Property
  | Ident "Reparent" -> Some Reparent
  | Ident "Unmap" -> Some Unmap
  | Ident "Visibility" -> Some Visibility
  | _ -> None


(* An event has modifiers and an event type *)
(* OLD:
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
*)


let parse_event_constituents stream =
  let rec collect mods =
    match Stream.peek stream with
    | Some tok ->
        begin match parse_modifier tok with
        | Some m ->
            Stream.junk stream;
            collect (m :: mods)
        | None ->
            begin match parse_event_type tok with
            | Some e ->
                Stream.junk stream;
                let real_e =
                  match e with
                  | ButtonPress ->
                      begin match Stream.peek stream with
                      | Some (Int n) ->
                          Stream.junk stream;
                          ButtonPressDetail n
                      | _ -> ButtonPress
                      end
                  | ButtonRelease ->
                      begin match Stream.peek stream with
                      | Some (Int n) ->
                          Stream.junk stream;
                          ButtonReleaseDetail n
                      | _ -> ButtonRelease
                      end
                  | KeyPress ->
                      begin match Stream.peek stream with
                      | Some (Ident s) ->
                          Stream.junk stream;
                          KeyPressDetail s
                      | _ -> KeyPress
                      end
                  | KeyRelease ->
                      begin match Stream.peek stream with
                      | Some (Ident s) ->
                          Stream.junk stream;
                          KeyReleaseDetail s
                      | _ -> KeyRelease
                      end
                  | _ -> e
                in
                (List.rev mods, real_e)
            | None ->
                begin match tok with
                | Int n ->
                    Stream.junk stream;
                    (List.rev mods, ButtonPressDetail n)
                | Ident s ->
                    Stream.junk stream;
                    (List.rev mods, KeyPressDetail s)
                | _ ->
                    raise (Parse_error "Invalid event")
                end
            end
        end
    | None ->
        raise (Parse_error "Unexpected end of stream")
  in
  collect []


(* OLD:
let event = parser
    [< 'StartEvent; e = event_constituents []; 'EndEvent >] -> e
*)

let parse_event stream =
  match Stream.peek stream with
  | Some StartEvent ->
      Stream.junk stream;
      let result = parse_event_constituents stream in
      expect stream EndEvent;
      result
  | _ ->
      raise (Parse_error "Expected <")


(* standard combinator *)
(* OLD:
let rec list_of p = parser
  | [< e = p ; l = list_of p>] -> e::l
  | [<>] -> []			 

let event_sequence = list_of event

let token_stream s = 
  Stream.from (fun n -> try Some (token s) with Stream.Failure -> None)

let parse_event s = 
  event (token_stream s)

let parse_event_sequence s = 
  event_sequence (token_stream s)

*)

let rec list_of p stream =
  match Stream.peek stream with
  | None -> []
  | Some _ ->
      try
        let e = p stream in
        e :: list_of p stream
      with Parse_error _ ->
        []

let parse_event_sequence stream =
  list_of parse_event stream

(*
let parse s =
  let stream = lexer s in
  parse_event stream
*)


(* This one is exported *)
let event_sequence name default =
  let s = get name in
  if s = "" then default
  else 
    try 
      (* parse_event_sequence (token_stream (Stream.of_string s)) *)
      parse_event_sequence (lexer s)
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

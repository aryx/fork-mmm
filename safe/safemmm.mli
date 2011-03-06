(* MMM specific run-time environment for applets *)

module Html : sig
  (* HTML tokens *)
  type attribute_name = string 
  type attribute_value = string
  type attributes = (attribute_name * attribute_value) list

  type tag = {
    tag_name : string;
    attributes: attributes
  }

  type token =
     PCData of string
   | CData of string
   | OpenTag of tag
   | CloseTag of string
   | Comment of string
   | Doctype of string
   | EOF

  type location = Loc of int * int

  exception Html_Lexing of string * int
  exception Invalid_Html of string

  val get_entity : string -> string
    (* [get_entity "amp"] returns "&" *)

  val get_attribute : tag -> string -> string
    (* [get_attribute tag attrib_name] *)

  val has_attribute : tag -> string -> bool
    (* [has_attribute tag attrib_name] *)

  (* HTML length *)
  type length = 
      Nolength
    | LengthPixels of int
    | LengthRatio of float
    | LengthRel of int

  val length_of_string : string -> length
end


module Dtd : sig
  type t
  val dtd20 : t
  val dtd32 : t
end

module ParseHTML : sig

 type minimization =
   Legal | Illegal of string

 val sgml_lexer :
  Dtd.t -> Lexing.lexbuf -> 
    ((string * int) list * minimization * Html.token list * Html.location)

end
  



type fontInfo = 
   Family of string
 | Weight of string
 | Slant of string
 | FontIndex of int
 | FontDelta of int

type gattr = 
     Margin of int
  |  Justification of string
  |  Font of fontInfo		        (* mostly size and face *)
  |  FgColor of string
  |  BgColor of string
  |  Underlined
  |  Striked
  |  Spacing of int
  |  Superscript
  |  Lowerscript

type objmap = 
    ClientSide of Hyper.link		(* usemap link *)
  | ServerSide of Hyper.link		(* ismap *)
  | Direct of Hyper.link			(* inside an anchor *)
  | NoMap				(* no additionnal navigation *)
  | FormMap of (int * int -> Hyper.link)


type embobject = {
  embed_hlink : Hyper.link;               (* hyperlink to the object *)
  embed_frame : Widget.widget;  
     (* the frame where the viewers can do their stuff *)
  embed_context : Viewers.context;
  embed_map : objmap;                  (* associated map *)
  embed_alt : string
 }

class  virtual imgloader : (unit) -> object
  method virtual add_image : embobject -> unit	 (* add one image *)
  method virtual flush_images : unit	         (* flush when document is loaded *)
  method virtual load_images : unit		 (* manual flush *)
  method virtual update_images : unit
end

type formatterSpec = 
    TopFormatter of bool		(* flag is pixel-scrolling mode *)
  | NestedFormatter
  | FrameFormatter of (string * string) list (* decoration ... *)

type formatter = {
  (* Text primitives of the device *)
  new_paragraph: unit -> unit;  	(* Open a new paragraph *)
    (* make sure the following text will start on a new line *)
  close_paragraph: unit -> unit;  	(* Close a paragraph *)
    (* make sure there is an eol after the current text *)
  print_newline : bool -> unit;		(* Force a line break *)
  print_verbatim : string -> unit;	(* Print as-is *)
  format_string : string -> unit;	(* Line wrap, newlines don't count *)
  flush : unit -> unit;			(* Flush the device *)
  (* Predefined Images *)
  hr : Html.length -> int -> bool -> unit;  (* [hr width height solid] *)
  bullet : string -> unit;
  (* Graphical attributes *)
  set_defaults : string -> gattr list -> unit;     (* bg, fg, links *)
  push_attr : gattr list -> unit;
  pop_attr : gattr list -> unit;
  (* Structure primitives *)
  isindex : string -> string -> unit;		(* <ISINDEX> *)
  start_anchor : unit -> unit;
  end_anchor : Hyper.link -> unit;
  add_mark : string -> unit;
  (* Embedding primitives *)
  create_embedded : 
     string -> int option -> int option -> Widget.widget;
       (* [create_embedded align w h ]: 
          returns a widget that we can pass as target to the embed manager.
          Should respect background color ?
        *)
  (* Re-centering on a fragment *)
  see_frag : string option -> unit
  }

class  virtual machine : (unit) -> object
  method virtual formatter : formatter
  method virtual imgmanager : imgloader
  method virtual base : string
  method virtual set_base : string -> unit
  method virtual target : string option
  method virtual set_target : string -> unit
  method virtual ctx : Viewers.context
  method virtual add_tag : string -> (formatter -> Html.tag -> unit) -> (formatter -> unit) -> unit
  method virtual get_tag : string -> (formatter -> Html.tag -> unit) * (formatter -> unit)
  method virtual remove_tag : string -> unit
  method virtual push_action : (string -> unit) -> unit
  method virtual pop_action : unit
  method virtual push_formatter : formatter -> unit
  method virtual pop_formatter : formatter
  method virtual create_formatter : formatterSpec -> Widget.widget -> formatter * Widget.widget
  method virtual send : Html.token -> unit
  method virtual look_for : Html.token -> unit
  method virtual add_embedded : embobject -> unit
  method virtual embedded : embobject list
  method virtual see_frag : string option -> unit

  (* For Japanese (or other langs) *)
  (* encode the internal i18n strings to corresponding encodings *)
  method virtual i18n_encoder : string -> string
  method virtual set_i18n_encoder : (string -> string) -> unit
end

module Get(C: sig val capabilities: Capabilities.t end) : sig
   val add_html_display_hook : (machine -> unit) -> unit
   val add_object : embobject -> unit
     (* queue an embedded object *)


   val add_user_navigation : string -> Viewers.hyper_func -> unit
   val add_user_menu : string -> (Viewers.context -> unit) -> unit

   (* test suit *)
   val add_html_filter : ((Html.token -> unit) -> Html.token -> unit) -> unit

   (* The following allows demonstrations/replay *)
   type navigator
   val new_window : Url.t -> navigator option
   val follow_link : navigator -> Hyper.link -> unit
   val destroy_window : navigator -> unit

   val new_window_initial : unit -> unit
   val new_window_sel : unit -> unit

   (* Embedded Viewers *)
   val add_embedded_viewer : 
     string * string ->
       ((string * string) list -> Widget.widget -> Viewers.context ->
	 Document.handle -> unit)
       -> unit
     (* [add_embedded_viewer media_type f]
	where [f media_parameters widget context document]
      *) 

   (* Retrieval machinery *)
  type retrievalStatus =
      Started of (unit -> unit)  | InUse

  type behaviour =
      Ok 
    | Stop of string
    | Retry of Hyper.link
    | Error of string
    | Restart of (Document.handle -> Document.handle)

  val add_http_handler : 
      int -> (Www.request -> Document.handle -> behaviour) -> unit

  val add_decoder : string -> (Document.handle -> Document.handle) -> unit

   (* Tachymeter support *)
   class  virtual tachymeter : (Widget.widget) -> object
     method virtual report_cnx : int -> unit     (* displays number of active cnx *)
     method virtual report_busy : bool -> unit   (* displays busy status *)
     method virtual report_traffic : int -> int -> int -> unit
	  (* [report_traffic tick_duration total sample] displays traffic
	     from [total] and [sample] in last [tick_duration] *)
     method virtual quit : unit
     end

   val set_tachy : (Widget.widget -> tachymeter) -> unit

end

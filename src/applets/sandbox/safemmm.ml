module Html = Html
module Dtd = Dtd
module ParseHTML = Html_eval

type fontInfo = Fonts.fontInfo =
   Family of string
 | Weight of string
 | Slant of string
 | FontIndex of int
 | FontDelta of int

type gattr = Htmlfmt.gattr =
     FgColor of string
  |  BgColor of string
  |  Font of fontInfo		        (* mostly size and face *)
  |  Margin of int
  |  Spacing of int
  |  Justification of string
  |  Underlined
  |  Striked
  |  Superscript
  |  Lowerscript


type objmap = Maps.t =
    ClientSide of Hyper.link		(* usemap link *)
  | ServerSide of Hyper.link		(* ismap *)
  | Direct of Hyper.link			(* inside an anchor *)
  | NoMap				(* no additionnal navigation *)
  | FormMap of (int * int -> Hyper.link)

type embobject = Embed.obj

class  virtual imgloader (unit : unit) =
 object
  method virtual add_image : < Cap.network > -> embobject -> unit (* add one image *)
  method virtual flush_images : unit	         (* flush when document is loaded *)
  method virtual load_images : unit		 (* manual flush *)
  method virtual update_images : < Cap.network > -> unit
end

type formatterSpec = Htmlfmt.formatterSpec =
    TopFormatter of bool		(* flag is pixel-scrolling mode *)
  | NestedFormatter
  | FrameFormatter of (string * string) list (* decoration ... *)

type formatter = Htmlfmt.formatter = {
  (* Text primitives of the device *)
  new_paragraph: unit -> unit;  	(* Open a new paragraph *)
    (* make sure the following text will start on a new line *)
  close_paragraph: unit -> unit;  	(* Close a paragraph *)
    (* make sure there is an eol after the current text *)
  print_verbatim : string -> unit;	(* Print as-is *)
  format_string : string -> unit;	(* Line wrap, newlines don't count *)
  print_newline : bool -> unit;		(* Force a line break *)
  (* Graphical attributes *)
  push_attr : gattr list -> unit;
  pop_attr : gattr list -> unit;
  set_defaults : string -> gattr list -> unit;     (* bg, fg, links *)
  (* Predefined Images *)
  bullet : string -> unit;
  hr : Html.length -> int -> bool -> unit;  (* [hr width height solid] *)
  (* Structure primitives *)
  isindex : string -> string -> unit;		(* <ISINDEX> *)
  add_mark : string -> unit;
  start_anchor : unit -> unit;
  end_anchor : Hyper.link -> unit;
  (* Embedding primitives *)
  create_embedded :
     string -> int option -> int option -> Widget.widget;
       (* [create_embedded align w h ]:
          returns a widget that we can pass as target to the embed manager.
          Should respect background color ?
        *)
  (* Re-centering on a fragment *)
  see_frag : string option -> unit;
  flush : unit -> unit;			(* Flush the device *)
  }

class  virtual machine (unit : unit) =
 object
  method virtual formatter : formatter
  method virtual imgmanager : Html_disp.imgloader
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


open Nav
open Document
open Capabilities
module Get(C: sig val capabilities: t end) = struct

  let ask = ask C.capabilities

(* Enhancements of the browser *)

(* Additions to the HTML display machine *)
  let add_html_display_hook f =
    if ask HTMLDisplay then Html_disp.add_hook (f :> machine -> unit)
    else raise (Sys_error (I18n.sprintf "Permission denied"))

(* With add_object, an applet can essentially capture the display of
   an embedded viewer into it's own widgets. This is of course of possible
   source of privacy violation because the viewer's widget for the document
   will be in the widget space of the applet.
   However, in the current version, the applet simply cannot access widgets
   it has not created (since we removed in Safetk all functions that return
   a widget).
   NOTE: check the bindings inheritance stuff ...
 *)
  let add_object = Embed.add
       
(* Add new items to the navigation menu popped up on anchors, or simply
   new navigation functions that will be propagated in "ctx" 
   NOTE: because of the construction order or hyper functions, it is not
   possible to override default navigation functions though. This could be
   a useful feature, but then it raises privacy/integrity issues...
 *) 
  let add_user_navigation = Nav.add_user_navigation
       
(* Add new items to the "User menu" *)
  let add_user_menu = Mmm.add_user_menu

(* Filtering HTML on the fly *)
  let add_html_filter f = 
    if ask HTMLDisplay then Html_eval.add_html_filter f
    else raise (Sys_error (I18n.sprintf "Permission denied"))

(* Navigation *)
  type navigator = Nav.t
  let new_window = Mmm.navigator (Cap.network_caps_UNSAFE ()) false
(* note: by construction (navigator is an abstract type, and all values
   of type navigator passed to the applets correspond to windows created
   by the applet), this means that the applet cannot destroy windows it has
   not created *)
  let destroy_window nav =
    Tk.destroy (Winfo.toplevel nav.nav_viewer_frame)

  let follow_link caps nav h = Nav.follow_link caps nav h
      
  let new_window_initial = Mmm.new_window_initial

(* Note: this accesses the X selection, but I don't think that it can leak
   through errors *)
  let new_window_sel = Mmm.new_window_sel

(* Add new embedded viewers : of course this requires read access to the
   documents that will be passed to this viewer 
   Note: the applet can override an existing viewer...
 *)
  let add_embedded_viewer s f =
    let real_f parms w ctx doc = 
      if ask (DocumentR (Url.string_of doc.document_address)) then
      	f parms w ctx (Cache.make_embed_handle doc)
      else
      	raise Denied in
    Embed.add_viewer s real_f


(* Retrieval machinery *)
  type retrievalStatus = Retrieve.status =
  	Started of (unit -> unit)  | InUse

  type behaviour = Retrieve.behaviour =
   	Ok 
    | Stop of string
    | Retry of Hyper.link
    | Error of string
    | Restart of (handle -> handle)

  let add_http_handler n h =
    if ask Internals then
      Retrieve.add_http_processor n h
    else raise Denied
      
(* Decoders *)
  let add_decoder encoding f =
    if ask Internals then
      Decoders.add encoding f
   else raise Denied

(* Tachymeter support *)
  class  virtual tachymeter (w : Widget.widget) =
 object
    method virtual report_cnx : int -> unit     (* displays number of active cnx *)
    method virtual report_busy : bool -> unit   (* displays busy status *)
    method virtual report_traffic : int -> int -> int -> unit
       (* [report_traffic tick_duration total sample] displays traffic
	  from [total] and [sample] in last [tick_duration] *)
    method virtual quit : unit
  end

  let set_tachy = Mmm.change_tachy
end

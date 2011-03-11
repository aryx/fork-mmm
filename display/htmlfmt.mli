(* HTML  "display device" *)
open Html
open Www
open Fonts
open Embed

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
  hr : length -> int -> bool -> unit;  (* [hr width height solid] *)
  bullet : string -> unit;
  (* Graphical attributes *)
  set_defaults : string -> gattr list -> unit;     (* bg, fg, links *)
  push_attr : gattr list -> unit;
  pop_attr : gattr list -> unit;
  (* Structure primitives *)
  isindex : string -> string -> unit;         (* <ISINDEX> *)
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
  see_frag : string option -> unit;
}

module type GfxHTML = sig
 val create : 
    (unit -> string) ->
    formatterSpec -> Widget.widget -> Viewers.context -> 
      formatter * Widget.widget
 end

(* Manager for in-lined images *)
module type ImgDisplay = sig
   (* Various configuration stuff *)
   type mode =
       DuringDoc
     | AfterDocAuto
     | AfterDocManual

   val mode : mode ref
   val no_images : bool ref

   class loader : (unit) -> object
     method add_image : embobject -> unit	 (* add one image *)
     method flush_images : unit	         (* flush when document is loaded *)
     method load_images : unit		 (* manual flush *)
     method update_images : unit
   end

   val create : unit -> loader
 end

(* Form manager *)
type input_kind = EntryInput | FileInput | OtherInput
    
class  virtual form_behaviour : (unit) -> object
    method virtual add_get : input_kind -> (unit -> (string * string) list) -> unit
    method virtual add_reset : (unit -> unit) -> unit
    method virtual submit : (string * string) list -> Hyper.link
    method virtual single_submit : Hyper.link option
    method virtual reset : unit
end

module type FormDisplay = sig
   (* A form manager *)
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

   val create : string -> form_behaviour -> Viewers.context -> t
      (* [create base behaviour ctx] *)

 end


(* Table manager *)
type width_constraint =
    TopWidth				(* toplevel window size*)
  | FixedWidth of int			(* width is given in pixels *)
  | UnknownWidth of (unit -> bool)	(* constraint to satisfy *)

module type TableDisplay = sig
    type cell_type = HeaderCell | DataCell
    type t = {
      table_master : Widget.widget;
      add_col : Html.tag -> unit;
      open_row : Html.tag -> unit;
      close_row : unit -> unit;
      close_table : unit -> unit;
      new_cell : 
	  cell_type -> Html.tag -> Widget.widget -> string -> width_constraint;
      bound : unit -> bool
      }

    val create : Widget.widget -> Html.tag -> width_constraint -> t

    val topwidth : Widget.widget -> int

 end
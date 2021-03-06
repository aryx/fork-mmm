(*s: ./display/htmlfmt.mli *)
(* HTML  "display device" *)

(*s: type Htmlfmt.gattr *)
type gattr =
  (*s: [[Htmlfmt.gattr]] color cases *)
  | FgColor of string
  (*x: [[Htmlfmt.gattr]] color cases *)
  | BgColor of string
  (*e: [[Htmlfmt.gattr]] color cases *)
  (*s: [[Htmlfmt.gattr]] font cases *)
  | Font of Fonts.fontInfo		        (* mostly size and face *)
  (*e: [[Htmlfmt.gattr]] font cases *)
  (*s: [[Htmlfmt.gattr]] spacing cases *)
  | Margin of int
  (*x: [[Htmlfmt.gattr]] spacing cases *)
  | Spacing of int
  (*e: [[Htmlfmt.gattr]] spacing cases *)
  (*s: [[Htmlfmt.gattr]] alignment cases *)
  | Justification of string
  (*e: [[Htmlfmt.gattr]] alignment cases *)
  (*s: [[Htmlfmt.gattr]] style cases *)
  | Underlined
  (*x: [[Htmlfmt.gattr]] style cases *)
  | Striked
  (*x: [[Htmlfmt.gattr]] style cases *)
  | Superscript
  (*x: [[Htmlfmt.gattr]] style cases *)
  | Lowerscript
  (*e: [[Htmlfmt.gattr]] style cases *)
(*e: type Htmlfmt.gattr *)

(*s: type Htmlfmt.formatterSpec *)
type formatterSpec = 
  | TopFormatter of bool		(* flag is pixel-scrolling mode *)
  | NestedFormatter
  | FrameFormatter of (string * string) list (* decoration ... *)
(*e: type Htmlfmt.formatterSpec *)

(*s: type Htmlfmt.formatter *)
type formatter = {

  (* Text primitives of the device *)
  (*s: [[Htmlfmt.formatter]] primitives methods *)
  new_paragraph: unit -> unit;  	(* Open a new paragraph *)
    (* make sure the following text will start on a new line *)
  close_paragraph: unit -> unit;  	(* Close a paragraph *)
    (* make sure there is an eol after the current text *)
  (*x: [[Htmlfmt.formatter]] primitives methods *)
  print_verbatim : string -> unit;	(* Print as-is *)
  (*x: [[Htmlfmt.formatter]] primitives methods *)
  format_string : string -> unit;	(* Line wrap, newlines don't count *)
  (*x: [[Htmlfmt.formatter]] primitives methods *)
  print_newline : bool -> unit;		(* Force a line break *)
  (*e: [[Htmlfmt.formatter]] primitives methods *)

  (* Graphical attributes *)
  (*s: [[Htmlfmt.formatter]] graphical attributes methods *)
  push_attr : gattr list -> unit;
  pop_attr : gattr list -> unit;
  (*x: [[Htmlfmt.formatter]] graphical attributes methods *)
  set_defaults : string -> gattr list -> unit;     (* bg, fg, links *)
  (*e: [[Htmlfmt.formatter]] graphical attributes methods *)

  (*s: [[Htmlfmt.formatter]] other methods *)
  (* Predefined Images *)
  (*s: [[Htmlfmt.formatter]] predefined images methods *)
  bullet : string -> unit;
  (*x: [[Htmlfmt.formatter]] predefined images methods *)
  hr : Html.length -> int -> bool -> unit;  (* [hr width height solid] *)
  (*e: [[Htmlfmt.formatter]] predefined images methods *)
  (* Structure primitives *)
  (*s: [[Htmlfmt.formatter]] structure primitives methods *)
  isindex : string -> string -> unit;         (* <ISINDEX> *)
  (*x: [[Htmlfmt.formatter]] structure primitives methods *)
  add_mark : string -> unit;
  (*x: [[Htmlfmt.formatter]] structure primitives methods *)
  start_anchor : unit -> unit;
  end_anchor : Hyper.link -> unit;
  (*e: [[Htmlfmt.formatter]] structure primitives methods *)
  (* Embedding primitives *)
  (*s: [[Htmlfmt.formatter]] embedding primitives methods *)
  create_embedded : 
     string -> int option -> int option -> Widget.widget;
       (* [create_embedded align w h ]: 
      returns a widget that we can pass as target to the embed manager.
      Should respect background color ?
    *)
  (*e: [[Htmlfmt.formatter]] embedding primitives methods *)
  (* Re-centering on a fragment *)
  (*s: [[Htmlfmt.formatter]] fragment method *)
  see_frag : string option -> unit;
  (*e: [[Htmlfmt.formatter]] fragment method *)
  (*e: [[Htmlfmt.formatter]] other methods *)

  flush : unit -> unit;			(* Flush the device *)
}
(*e: type Htmlfmt.formatter *)


(*s: type Htmlfmt.input_kind *)
(* Form manager *)
type input_kind = EntryInput | FileInput | OtherInput
(*e: type Htmlfmt.input_kind *)
    
class  virtual form_behaviour : (unit) -> object
    method virtual add_get : input_kind -> (unit -> (string * string) list) -> unit
    method virtual add_reset : (unit -> unit) -> unit
    method virtual submit : (string * string) list -> Hyper.link
    method virtual single_submit : Hyper.link option
    method virtual reset : unit
end

(*s: type Htmlfmt.width_constraint *)
(* Table manager *)
type width_constraint =
  | TopWidth				(* toplevel window size*)
  | FixedWidth of int			(* width is given in pixels *)
  | UnknownWidth of (unit -> bool)	(* constraint to satisfy *)
(*e: type Htmlfmt.width_constraint *)

(*
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
*)

(*
module type FormDisplay = sig
   (* A form manager *)
   type t = {
     text_input : Widget.widget -> Html.tag -> unit;
     (* [text_input top tag] *)
     checkbox_input : Widget.widget ->  Html.tag -> unit;
     (* [input top tag] *)
     radio_input : Widget.widget ->  Html.tag -> unit;
     (* [input top tag] *)
     image_input : Widget.widget ->  Html.tag -> Embed.embobject;
     (* [input top tag] *)
     submit_input : Widget.widget ->  Html.tag -> unit;
     (* [input top tag] *)
     reset_input : Widget.widget ->  Html.tag -> unit;
     (* [input top tag] *)
     select : Widget.widget -> (string * string * bool) list -> Html.tag -> unit;
     (* [select top elements tag] *)
     textarea:  Widget.widget -> string -> Html.tag -> unit
     (* [textarea top initial attrs] *)
     }

   val create : string -> form_behaviour -> Viewers.context -> t
      (* [create base behaviour ctx] *)

 end
*)

(*
module type GfxHTML = sig
 val create : 
    (unit -> string) ->
    formatterSpec -> Widget.widget -> Viewers.context -> 
      formatter * Widget.widget
 end
*)

(*e: ./display/htmlfmt.mli *)

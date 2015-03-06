(*s: ./viewers/viewers.mli *)
open Www
open Document
open Http_headers

(*s: type Viewers.vparams *)
(* hyper functions are: "goto", "save", "gotonew" *)
type vparams = (string * string) list
(*e: type Viewers.vparams *)
(*s: type Viewers.frame_targets *)
type frame_targets = (string * Widget.widget) list
(*e: type Viewers.frame_targets *)

(*s: signature Viewers.frame_adopt *)
val frame_adopt : Widget.widget -> frame_targets -> frame_targets
    (* remap _self and _parent *)
(*e: signature Viewers.frame_adopt *)
(*s: signature Viewers.frame_fugue *)
val frame_fugue : frame_targets -> frame_targets
    (* forget about _self and _parents *)
(*e: signature Viewers.frame_fugue *)

(*s: type Viewers.hyper_func *)
type hyper_func = {
  hyper_visible : bool;
  hyper_title : string;
  hyper_func : frame_targets -> Hyper.link -> unit
  }
(*e: type Viewers.hyper_func *)

(* list of additionnal parameters for the viewer, according to its
   activation point *)

(*s: signature class Viewers.context *)
(* The context given to a viewer *)
(* Standard hyper functions are: "goto", "save", "gotonew" *)
class virtual context : (Document.document_id * vparams) -> object ('a)

  method base : Document.document_id
  method params : vparams

  method goto : Hyper.link -> unit
  method gotonew : Hyper.link -> unit
  method save : Hyper.link -> unit
  method invoke : string -> Hyper.link -> unit    

  method virtual log : string -> unit
  method add_nav : string * hyper_func -> unit
  (*-*)
  method for_embed : vparams -> frame_targets -> 'a
  method in_embed : Document.document_id -> 'a
  method hyper_funs : (string * hyper_func) list

end
(*e: signature class Viewers.context *)

(*s: signature class Viewers.display_info *)
class  virtual display_info : (unit) -> object ('a)
  (*s: [[Viewers.display_info]] virtual fields signatures *)
  method virtual di_title : string		(* some visible title *)
  method virtual di_fragment : string option -> unit	(* for # URIs *)

  method virtual di_widget : Widget.widget

  method virtual di_abort : unit		(* stop display *)
  method virtual di_destroy : unit		(* die *)


  method virtual di_redisplay : unit		(* redisplay *)
  method virtual di_load_images : unit	        (* load images *)
  method virtual di_update : unit      (* update embedded objects *)
  (*x: [[Viewers.display_info]] virtual fields signatures *)
  method virtual di_source : unit 	        (* source viewer *)
  (*e: [[Viewers.display_info]] virtual fields signatures *)
  (*s: [[Viewers.display_info]] cache methods *)
  method di_last_used : int
  method di_touch : unit
  (*e: [[Viewers.display_info]] cache methods *)
end
(*e: signature class Viewers.display_info *)



class trivial_display : (Widget.widget * Url.t) -> (* #display_info *)
(* boilerplate class type *)
object
  method di_abort : unit
  method di_destroy : unit
  method di_fragment : string option -> unit
  method di_last_used : int
  method di_load_images : unit
  method di_redisplay : unit
  method di_source : unit
  method di_title : string
  method di_touch : unit
  method di_widget : Widget.widget
  method di_update : unit
end

(*s: signature Viewers.di_compare *)
val di_compare : display_info -> display_info -> bool
(*e: signature Viewers.di_compare *)

(*s: type Viewers.t *)
(* Definition of an internal viewer *)
type t = 
    Http_headers.media_parameter list -> 
    Widget.widget -> 
    context -> 
    Document.handle -> 
    display_info option
(*e: type Viewers.t *)

(*s: signature Viewers.add_viewer *)
val add_viewer : media_type -> t -> unit
    (* [add_viewer type viewer] *)
(*e: signature Viewers.add_viewer *)

(*s: signature Viewers.add_builtin *)
val add_builtin : media_type -> t -> unit
    (* [add_builtin type viewer] makes viewer a builtin for type *)
(*e: signature Viewers.add_builtin *)

(*s: signature Viewers.rem_viewer *)
val rem_viewer : media_type -> unit
(*e: signature Viewers.rem_viewer *)

(*s: signature Viewers.view *)
val view : Widget.widget -> context -> Document.handle -> display_info option
(*e: signature Viewers.view *)

(*s: signature Viewers.reset *)
val reset : unit -> unit
(*e: signature Viewers.reset *)
(*e: ./viewers/viewers.mli *)

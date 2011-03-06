open Www
open Document
open Http_headers

type vparams = (string * string) list
type frame_targets = (string * Widget.widget) list

val frame_adopt : Widget.widget -> frame_targets -> frame_targets
    (* remap _self and _parent *)
val frame_fugue : frame_targets -> frame_targets
    (* forget about _self and _parents *)

type hyper_func = {
  hyper_visible : bool;
  hyper_title : string;
  hyper_func : frame_targets -> Hyper.link -> unit
  }

(* list of additionnal parameters for the viewer, according to its
   activation point *)
  
(* The context given to a viewer *)
(* Standard hyper functions are: "goto", "save", "gotonew" *)
class  virtual context : (Document.document_id * vparams) -> object ('a)
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

class  virtual display_info : (unit) -> object ('a)
  method virtual di_widget : Widget.widget
  method virtual di_abort : unit		(* stop display *)
  method virtual di_destroy : unit		(* die *)
  method virtual di_fragment : string option -> unit	(* for # URIs *)
  method virtual di_redisplay : unit		(* redisplay *)
  method virtual di_title : string		(* some visible title *)
  method virtual di_source : unit 	        (* source viewer *)
  method virtual di_load_images : unit	        (* load images *)
  method virtual di_update : unit
  method di_last_used : int
  method di_touch : unit
end


class trivial_display : (Widget.widget * Url.t) -> object
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


val di_compare : display_info -> display_info -> bool

type t = 
    media_parameter list -> Widget.widget -> context -> handle 
          -> display_info option

val add_viewer : media_type -> t -> unit
    (* [add_viewer type viewer] *)

val add_builtin : media_type -> t -> unit
    (* [add_builtin type viewer] makes viewer a builtin for type *)

val rem_viewer : media_type -> unit

val view : Widget.widget -> context -> handle -> display_info option

val reset : unit -> unit

(*s: viewers/viewers.mli *)

(*s: type [[Viewers.vparams]] *)
(* list of additionnal parameters for the viewer, according to its
   activation point *)
(* hyper functions are: "goto", "save", "gotonew" *)
type vparams = (string * string) list
(*e: type [[Viewers.vparams]] *)
(*s: type [[Viewers.frame_targets]] *)
type frame_targets = (string * Widget.widget) list
(*e: type [[Viewers.frame_targets]] *)

(*s: type [[Viewers.hyper_func]] *)
type hyper_func = {
  hyper_visible : bool;
  hyper_title : string;

  hyper_func : frame_targets -> Hyper.link -> unit
}
(*e: type [[Viewers.hyper_func]] *)

(*s: signature class [[Viewers.context]] *)
(* The context given to a viewer *)
class virtual context : (Document.id * vparams) -> object ('a)

  method base : Document.id

  (*s: [[Viewers.context]] hypertext methods signatures *)
  method goto    : Hyper.link -> unit
  method gotonew : Hyper.link -> unit
  method save    : Hyper.link -> unit
  (*x: [[Viewers.context]] hypertext methods signatures *)
  method invoke  : string -> Hyper.link -> unit    
  method add_nav    : string * hyper_func -> unit
  method hyper_funs : (string * hyper_func) list
  (*e: [[Viewers.context]] hypertext methods signatures *)
  (*s: [[Viewers.context]] embedded methods signatures *)
  (*-*)
  method for_embed : vparams -> frame_targets -> 'a
  method in_embed : Document.id -> 'a
  (*e: [[Viewers.context]] embedded methods signatures *)

  (*s: [[Viewers.context]] logging methods signatures *)
  method virtual log : string -> unit
  (*e: [[Viewers.context]] logging methods signatures *)
  (*s: [[Viewers.context]] other methods signatures *)
  method params : vparams
  (*e: [[Viewers.context]] other methods signatures *)
end
(*e: signature class [[Viewers.context]] *)

(*s: signature class [[Viewers.display_info]] *)
class  virtual display_info : (unit) -> object ('a)

  (*s: [[Viewers.display_info]] virtual methods signatures *)
  method virtual di_title : string		(* some visible title *)

  (* the created widget containing the graphics *)
  method virtual di_widget : Widget.widget

  (*s: [[Viewers.display_info]] images virtual methods signatures *)
  method virtual di_load_images : unit (* load images *)
  (*e: [[Viewers.display_info]] images virtual methods signatures *)
  (*s: [[Viewers.display_info]] embedded virtual methods signatures *)
  method virtual di_update : unit      (* update embedded objects *)
  (*e: [[Viewers.display_info]] embedded virtual methods signatures *)
  (*s: [[Viewers.display_info]] fragment virtual method signature *)
  method virtual di_fragment : string option -> unit	(* for # URIs *)
  (*e: [[Viewers.display_info]] fragment virtual method signature *)

  (*s: [[Viewers.display_info]] lifecycle virtual methods signatures *)
  method virtual di_abort : unit		 (* stop display *)
  (*x: [[Viewers.display_info]] lifecycle virtual methods signatures *)
  method virtual di_redisplay : unit		(* redisplay *)
  (*e: [[Viewers.display_info]] lifecycle virtual methods signatures *)

  (*s: [[Viewers.display_info]] graphic cache virtual methods signatures *)
  method virtual di_destroy : unit	 (* die *)
  (*e: [[Viewers.display_info]] graphic cache virtual methods signatures *)
  (*s: [[Viewers.display_info]] other virtual methods signatures *)
  method virtual di_source : unit 	        (* source viewer *)
  (*e: [[Viewers.display_info]] other virtual methods signatures *)
  (*e: [[Viewers.display_info]] virtual methods signatures *)
  (*s: [[Viewers.display_info]] graphic cache methods signatures *)
  method di_touch : unit
  method di_last_used : int
  (*e: [[Viewers.display_info]] graphic cache methods signatures *)
end
(*e: signature class [[Viewers.display_info]] *)


(*s: signature [[Viewers.di_compare]] *)
val di_compare : display_info -> display_info -> int
(*e: signature [[Viewers.di_compare]] *)

(*s: type [[Viewers.t]] *)
(* Definition of an internal viewer *)
type t = 
    Http_headers.media_parameter list -> 
    (Widget.widget -> context -> Document.handle -> display_info option)
(*e: type [[Viewers.t]] *)

(*s: signature [[Viewers.add_viewer]] *)
val add_viewer : Http_headers.media_type -> t -> unit
    (* [add_viewer type viewer] *)
(*e: signature [[Viewers.add_viewer]] *)
(*s: signature [[Viewers.rem_viewer]] *)
val rem_viewer : Http_headers.media_type -> unit
(*e: signature [[Viewers.rem_viewer]] *)
(*s: signature [[Viewers.add_builtin]] *)
val add_builtin : Http_headers.media_type -> t -> unit
    (* [add_builtin type viewer] makes viewer a builtin for type *)
(*e: signature [[Viewers.add_builtin]] *)
(*s: signature [[Viewers.reset]] *)
val reset : unit -> unit
(*e: signature [[Viewers.reset]] *)

(* !!! main entry point!!!! *)
(*s: signature [[Viewers.view]] *)
val f : Widget.widget -> context -> Document.handle -> display_info option
(*e: signature [[Viewers.view]] *)

(*s: signature [[Viewers.frame_adopt]] *)
val frame_adopt : Widget.widget -> frame_targets -> frame_targets
    (* remap _self and _parent *)
(*e: signature [[Viewers.frame_adopt]] *)
(*s: signature [[Viewers.frame_fugue]] *)
val frame_fugue : frame_targets -> frame_targets
    (* forget about _self and _parents *)
(*e: signature [[Viewers.frame_fugue]] *)
(*e: viewers/viewers.mli *)

(*s: ./gui/nav.mli *)
(*s: type Nav.t *)
type t = {
  nav_viewer_frame : Widget.widget;
  (*s: [[Nav.t]] other fields *)
  nav_show_current: Viewers.display_info -> string option -> unit;
  (*x: [[Nav.t]] other fields *)
  nav_add_hist : Document.document_id -> string option -> unit;
  (*x: [[Nav.t]] other fields *)
  nav_id : int;  (* key for the gfx cache *)
  (*x: [[Nav.t]] other fields *)
  nav_log : string -> unit;
  (*x: [[Nav.t]] other fields *)
  nav_error : Error.t;			(* popping error dialogs *)
  (*x: [[Nav.t]] other fields *)
  nav_add_active : Url.t -> (unit -> unit) -> unit;
  nav_rem_active : Url.t -> unit;
  (*x: [[Nav.t]] other fields *)
  nav_new : Hyper.link -> unit;
  (*e: [[Nav.t]] other fields *)
 }
(*e: type Nav.t *)

(*s: signature Nav.request *)
val request :
  t -> (t -> Www.request -> Document.handle -> unit) ->
  ( bool * 
    (Www.request -> Www.request) * 
    (t -> Document.document_id -> Www.request -> unit)
  ) ->
  Hyper.link -> 
  unit
(*e: signature Nav.request *)

(*s: signature Nav.display_headers *)
val display_headers : Document.handle -> unit
(*e: signature Nav.display_headers *)

(*s: signature Nav.copy_link *)
val copy_link : t -> Hyper.link -> unit
(*e: signature Nav.copy_link *)
(*s: signature Nav.save_link *)
val save_link : t -> (Unix.file_descr * bool) option -> Hyper.link -> unit
(*e: signature Nav.save_link *)
(*s: signature Nav.follow_link *)
val follow_link : t -> Hyper.link -> unit
(*e: signature Nav.follow_link *)

(*s: signature Nav.add_user_navigation *)
val add_user_navigation : string -> Viewers.hyper_func -> unit
(*e: signature Nav.add_user_navigation *)

(*s: signature Nav.make_ctx *)
val make_ctx : t -> Document.document_id -> Viewers.context
(*e: signature Nav.make_ctx *)

(*s: signature Nav.absolutegoto *)
val absolutegoto : t -> string -> unit
(*e: signature Nav.absolutegoto *)
(*s: signature Nav.historygoto *)
val historygoto : t -> Document.document_id -> string option -> bool -> bool
(*e: signature Nav.historygoto *)
(*s: signature Nav.update *)
val update : t -> Document.document_id -> bool -> unit
(*e: signature Nav.update *)

(*s: signature Nav.dont_check_cache *)
(*-*)
val dont_check_cache : Www.request -> bool
(*e: signature Nav.dont_check_cache *)
(*e: ./gui/nav.mli *)

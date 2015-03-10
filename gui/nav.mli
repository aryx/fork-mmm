(*s: ./gui/nav.mli *)
(*s: type Nav.t *)
type t = {
  nav_viewer_frame : Widget.widget;

  (* Nav.absolutegoto -> request -> process_viewer -> <> *)
  nav_show_current: Viewers.display_info -> string option -> unit;

  (*s: [[Nav.t]] manage history methods *)
  nav_add_hist : Document.document_id -> string option -> unit;
  (*e: [[Nav.t]] manage history methods *)
  (*s: [[Nav.t]] manage active connections methods *)
  nav_add_active : Url.t -> Www.aborter -> unit;
  nav_rem_active : Url.t -> unit;
  (*e: [[Nav.t]] manage active connections methods *)

  (*s: [[Nav.t]] cache related methods *)
  nav_id : int;  (* key for the gfx cache *)
  (*e: [[Nav.t]] cache related methods *)
  (*s: [[Nav.t]] error methods *)
  nav_error : Error.t;			(* popping error dialogs *)
  (*e: [[Nav.t]] error methods *)
  (*s: [[Nav.t]] logging method *)
  nav_log : string -> unit;
  (*e: [[Nav.t]] logging method *)

  (*s: [[Nav.t]] other fields *)
  nav_new : Hyper.link -> unit;
  (*e: [[Nav.t]] other fields *)
 }
(*e: type Nav.t *)

(*s: signature Nav.request *)
val request :
  t -> (t -> Www.request -> Document.handle -> unit) ->
  (*s: [[Nav.request]] signature, extra arguments *)
  ( bool * 
    (Www.request -> Www.request) * 
    (t -> Document.document_id -> Www.request -> unit)
  ) ->
  (*e: [[Nav.request]] signature, extra arguments *)
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

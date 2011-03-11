type t =
  { nav_id: int;
    nav_viewer_frame: Widget.widget;
    nav_error: Error.t;
    nav_add_hist: Document.document_id -> string option -> unit;
    nav_show_current: Viewers.display_info -> string option -> unit;
    nav_log: string -> unit;
    nav_new: Hyper.link -> unit;
    nav_add_active: Url.t -> (unit -> unit) -> unit;
    nav_rem_active: Url.t -> unit }

val request :
  t ->
  bool ->
  (Www.request -> Www.request) ->
  (t -> Www.request -> Document.handle -> unit) ->
  (t -> Document.document_id -> Www.request -> unit) -> Hyper.link -> unit

val display_headers : Document.handle -> unit

val copy_link : t -> Hyper.link -> unit
val save_link : t -> (Unix.file_descr * bool) option -> Hyper.link -> unit
val follow_link : t -> Hyper.link -> unit

val add_user_navigation : string -> Viewers.hyper_func -> unit

val make_ctx : t -> Document.document_id -> Viewers.context

val absolutegoto : t -> string -> unit
val historygoto : t -> Document.document_id -> string option -> bool -> bool
val update : t -> Document.document_id -> bool -> unit

(*-*)
val dont_check_cache : Www.request -> bool

(*s: ./display/htmlw.mli *)

(*s: signature Htmlw.frames_as_links *)
val frames_as_links : bool ref
(*e: signature Htmlw.frames_as_links *)
(*s: signature Htmlw.pscrolling *)
val pscrolling : bool ref
(*e: signature Htmlw.pscrolling *)
(*s: signature Htmlw.ignore_meta_charset *)
val ignore_meta_charset : bool ref
(*e: signature Htmlw.ignore_meta_charset *)

class  virtual viewer_globs : (Viewers.context * Document.handle) -> object
  (* val ctx : Viewers.context *)
  val mutable dh : Document.handle
  val did : Document.document_id
  method ctx : Viewers.context
  method dh : Document.handle
  method did : Document.document_id
end

(*s: signature Htmlw.progress_report *)
val progress_report : 
    Widget.widget -> Viewers.context -> Widget.widget * Scheduler.progress_func
(*e: signature Htmlw.progress_report *)

(*s: signature Htmlw.html_head_ui *)
val html_head_ui :
    string list -> (unit -> unit) -> bool ref -> Widget.widget ->
      Viewers.context ->
    Widget.widget * (string -> unit) * (string -> Hyper.link -> unit) *
    (string -> string -> unit) * ((Widget.widget -> unit) -> unit)
(* [html_head_ui headers redisplay scrollmode top ctx]
   returns 
   hgroup, set_title, add_link, add_header, add_extra_header
*)
(*e: signature Htmlw.html_head_ui *)
(*s: signature Htmlw.display_html *)
(* [html_head_ui headers redisplay scrollmode top ctx]
   returns 
   hgroup, set_title, add_link, add_header, add_extra_header
*)

val display_html :
  Http_headers.media_parameter list ->
  Widget.widget ->
  Viewers.context -> Document.handle -> Viewers.display_info option
(*e: signature Htmlw.display_html *)
(*e: ./display/htmlw.mli *)

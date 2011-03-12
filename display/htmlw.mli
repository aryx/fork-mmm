
val frames_as_links : bool ref
val pscrolling : bool ref
val ignore_meta_charset : bool ref

class  virtual viewer_globs : (Viewers.context * Document.handle) -> object
  (* val ctx : Viewers.context *)
  val mutable dh : Document.handle
  val did : Document.document_id
  method ctx : Viewers.context
  method dh : Document.handle
  method did : Document.document_id
end

val progress_report : 
    Widget.widget -> Viewers.context -> Widget.widget * Scheduler.progress_func

val html_head_ui :
    string list -> (unit -> unit) -> bool ref -> Widget.widget ->
      Viewers.context ->
    Widget.widget * (string -> unit) * (string -> Hyper.link -> unit) *
    (string -> string -> unit) * ((Widget.widget -> unit) -> unit)
(* [html_head_ui headers redisplay scrollmode top ctx]
   returns 
   hgroup, set_title, add_link, add_header, add_extra_header
*)

val display_html :
  Http_headers.media_parameter list ->
  Widget.widget ->
  Viewers.context -> Document.handle -> Viewers.display_info option

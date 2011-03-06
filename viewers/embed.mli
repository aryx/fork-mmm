module EmbeddedScheduler : Scheduler.S with
  type shared_data = Document.document

val add_viewer : 
  Http_headers.media_type -> 
  (Http_headers.media_parameter list -> Widget.widget -> Viewers.context ->
    Document.document -> unit) -> unit

val rem_viewer :  Http_headers.media_type -> unit

(* Embedded objects *)
type embobject = {
  embed_hlink : Hyper.link;               (* hyperlink to the object *)
  embed_frame : Widget.widget;  
     (* the frame where the viewers can do their stuff *)
  embed_context : Viewers.context;
  embed_map : Maps.t;                  (* associated map *)
  embed_alt : string
 }

val add : embobject -> unit
val update : 
    Widget.widget -> Viewers.context -> Document.document -> (unit -> unit)
	-> unit

(*s: viewers/embed.mli *)
module EmbeddedScheduler : Scheduler.S with
  type shared_data = Document.t

(*s: type [[Embed.viewer]] *)
type viewer = 
  Http_headers.media_parameter list -> 
  Widget.widget -> Viewers.context -> Document.t -> unit
(*e: type [[Embed.viewer]] *)

(*s: signature [[Embed.add_viewer]] *)
val add_viewer : 
  Http_headers.media_type -> viewer -> unit
(*e: signature [[Embed.add_viewer]] *)

(*s: signature [[Embed.rem_viewer]] *)
val rem_viewer :  Http_headers.media_type -> unit
(*e: signature [[Embed.rem_viewer]] *)

(*s: type [[Embed.embobject]] *)
(* Embedded objects *)
type obj = {
  embed_hlink : Hyper.link; (* hyperlink to the object *)
  embed_frame : Widget.widget;
      (* the frame where the viewers can do their stuff *)
  embed_context : Viewers.context;
  embed_map : Maps.t; (* associated map *)
  embed_alt : string;
}
(*e: type [[Embed.embobject]] *)

(*s: signature [[Embed.add]] *)
val add : < Cap.network; ..> ->
  obj -> unit
(*e: signature [[Embed.add]] *)
(*s: signature [[Embed.update]] *)
val update : < Cap.network; ..> ->
  Widget.widget -> Viewers.context -> Document.t -> (unit -> unit) -> unit
(*e: signature [[Embed.update]] *)
(*e: viewers/embed.mli *)

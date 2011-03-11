open Html
open Htmlfmt

val form_bg : string ref

type t = {
  text_input : Widget.widget -> tag -> unit;
      (* [text_input top tag] *)
  checkbox_input : Widget.widget ->  tag -> unit;
      (* [input top tag] *)
  radio_input : Widget.widget ->  tag -> unit;
      (* [input top tag] *)
  image_input : Widget.widget ->  tag -> Embed.embobject;
      (* [input top tag] *)
  submit_input : Widget.widget ->  tag -> unit;
      (* [input top tag] *)
  reset_input : Widget.widget ->  tag -> unit;
      (* [input top tag] *)
  select : Widget.widget -> (string * string * bool) list -> tag -> unit;
      (* [select top elements tag] *)
  textarea:  Widget.widget -> string -> tag -> unit
      (* [textarea top initial attrs] *)
  }

val create : string -> form_behaviour -> Viewers.context -> t


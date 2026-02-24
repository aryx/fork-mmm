(*s: display/hr.ml *)
open Tk
open Html

(*s: function [[Hr.create_named]] *)
(* When creating an HR in a nested window (eg table cell), reqwidth is
   probably the width of 1 character
 *)
let create_named top name length height solid =
  let fr = Frame.create_named top name [] in
  let width = match length with
    Nolength | LengthRel _-> truncate (float (Winfo.reqwidth top) *. 0.95)
  | LengthRatio r -> truncate (float (Winfo.reqwidth top) *. r)
  | LengthPixels n -> n
  in
  Frame.configure fr [Width (Pixels width)];
  if solid then
    Frame.configure fr [BorderWidth (Pixels 0); Height (Pixels height)]
  else
    Frame.configure fr [Relief Groove;
            BorderWidth (Pixels 2); Height (Pixels (height+2))];
  fr
(*e: function [[Hr.create_named]] *)
(*e: display/hr.ml *)

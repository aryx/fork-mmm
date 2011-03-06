open Tk
open Protocol

module StringSet = Set.Make(struct type t = string let compare = compare end)

(* should we keep a negative cache ? *)
let available_colors = ref (StringSet.empty)

let check s =
  if StringSet.mem s !available_colors then true
  else begin
    try
      let f = Frame.create_named Widget.default_toplevel "frxcolorcheck" 
          [Background (NamedColor s)] in
      available_colors := StringSet.add s !available_colors;
      destroy f;
      true
    with
      TkError _ -> false
  end

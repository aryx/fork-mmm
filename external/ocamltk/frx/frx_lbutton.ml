open Tk

open Widget


let version = "$Id: frx_lbutton.ml,v 1.1 1996/10/22 15:55:32 rouaix Exp $"

(*
 * Simulate a button with a bitmap AND a label
 *)

let rec sort_options but lab com = function
    [] -> but,lab,com
  |(Command f as o)::l -> sort_options (o::but) lab com l
  |(Bitmap b as o)::l -> sort_options (o::but) lab com l
  |(Text t as o)::l -> sort_options but (o::lab) com l
  |o::l -> sort_options but lab (o::com) l

let create parent options =
  let but,lab,com = sort_options [] [] [] options in
  let f = Frame.create parent com in
  let b = Button.create f (but@com)
  and l = Label.create f (lab@com) in
    pack [b;l][];
    bind l [[],ButtonPressDetail 1] (BindSet ([],(function _ -> Button.invoke b)));
    f

let configure f options =
  let but,lab,com = sort_options [] [] [] options in
  match Pack.slaves f with
    [b;l] ->
      Frame.configure f com;
      Button.configure b (but@com);
      Label.configure l (lab@com)
  | _ -> raise (Invalid_argument "lbutton configure")

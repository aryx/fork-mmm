open Tk

let version = "$Id: frx_entry.ml,v 1.1 1996/10/22 15:55:28 rouaix Exp $"

(*
 * Tk 4.0 has emacs bindings for entry widgets
 *)

let new_label_entry parent txt action =
  let f = Frame.create parent [] in
  let m = Label.create f [Text txt]
  and e = Entry.create f [Relief Sunken; TextWidth 0] in
   Tk.bind e [[], KeyPressDetail "Return"] 
       (BindSet ([], fun _ -> action(Entry.get e)));
  pack [m][Side Side_Left];
  pack [e][Side Side_Right; Fill Fill_X; Expand true];
  f,e

let new_labelm_entry parent txt memo =
  let f = Frame.create parent [] in
  let m = Label.create f [Text txt]
  and e = Entry.create f [Relief Sunken; TextVariable memo; TextWidth 0] in
  pack [m][Side Side_Left];
  pack [e][Side Side_Right; Fill Fill_X; Expand true];
  f,e



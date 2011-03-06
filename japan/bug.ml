(* open Tk *)

let wow e =
  if e = Sys.Break then raise Sys.Break;
(*
  let top = Toplevel.create_named Widget.default_toplevel "japanesebug" [Class "Dialog"] in
  Wm.title_set top "Wow! You find a bug!";
  let m1 = Message.create top [Text "You finally find a bug of my Japanese lexing library. Please report me what page you have tried to display and the error message below. Thanks in advance. --- JPF"; Width (Pixels 400)] 
  and m2 = Message.create top [Text (Printexc.to_string e); Width (Pixels 300)] 
  and b = Button.create top [Text "Done"]
  in
  pack [m1;m2;b] [Side Side_Top];
  Button.configure b [Command (fun () -> destroy top)]

*)
Log.f "You finally find a bug of my Japanese lexing library. Please report me what page you have tried to display and the error message below. Thanks in advance. --- JPF";
Log.f (Printexc.to_string e)

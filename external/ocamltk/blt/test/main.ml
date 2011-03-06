open Protocol
open Tk
open Blt

let source top =
  let t = Toplevel.create top [] in
  Wm.title_set t "Source window";
  (* We are preparing an entry widget from which you can copy data *)
  let e = Entry.create t [] in
  DragDrop.create_source e [
     Button 3; (* 1 and 2 are already used *)
     PackageCmd (function token ->
       let pick = Entry.get e in
       (match Winfo.children token with
	 [] -> pack [Label.create token [Text pick]][]
       | [x] -> Label.configure x [Text pick]);
       tkreturn pick);  (* this is passed to the handler defined below*)
     Send ["text"] 
     ];
  DragDrop.source_handler e "text" (BuiltinHandler "dd_send_text");
  pack [e][]

let target top =
  let t = Toplevel.create top [] in
  Wm.title_set t "Target window";
  (* We are preparing a label widget into which you can copy data *)
  let l = Label.create t [Text "Initial Text"] in
  DragDrop.target_handler l "text" 
    (function () ->
       (* this is a bit magic because you need to know the protocol ! *)
       let txt = Textvariable.get (Textvariable.coerce "DragDrop(text)") in
       Label.configure l [Text txt]);
  pack [l][]

let main () =
  let top = openTk() in
  Blt.init();
  source top;
  source top;
  target top;
  mainLoop()

let _ = Printexc.print main ()

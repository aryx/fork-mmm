open Tk
open Widget
open Balloon
open Protocol

let _ =
let t = openTk () in
Balloon.init ();
let b = Button.create t [Text "hello"] in
Button.configure b [Command (fun () -> destroy b)];
pack [b][];
Balloon.put b 1000 "Balloon";
Printexc.catch mainLoop ()
 

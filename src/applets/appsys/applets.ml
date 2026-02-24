(***********************************************************************)
(*                                                                     *)
(*                           Calves                                    *)
(*                                                                     *)
(*          Francois Rouaix, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

type applet_callback = Widget.widget -> Viewers.context -> unit

(* Oups. This is defined in Dload *)
let register name cb = 
  if !Dload.in_load then Dload.register name cb

(* [error frame errmsg] reports an Error in applet evaluation (that is,
   in evaluation of entry points, since callbacks are on a different 
   thread of control). [frame] is the applet frame. *)
let error frame msg =
  let t = I18n.sprintf "Applet Error: %s" msg in
  Tk.pack[Label.create frame [Text t]][]

(* [call table frame context]
   calls the main entry point of an applet
   [table] : table of entry points registered by a bytecode file
   [frame] : applet frame
   [context] : Viewers.context
     viewer_params contains attributes of the EMBED tag
*)
let call table frame ctx =
  let fname = 
    try
      List.assoc "function" ctx#params
    with
      Not_found -> "main" (* default entry point *) in
  try
    let foo = Hashtbl.find table fname in
    (* destroy the alt window of the frame*)
    List.iter Tk.destroy (Winfo.children frame);
    try 
      Printexc.print (foo frame) ctx
    with
      e -> error frame 
	  (I18n.sprintf "Applet function \"%s\" raised exception: %s"
	     fname (Printexc.to_string e))
  with
    Not_found ->
      (* mismatch between EMBED function= and registered entry points *)
      error frame (I18n.sprintf "Applet function \"%s\" not found" fname)

(* Support for "external" windows *)
let get_toplevel_widget options = 
  Toplevel.create Widget.default_toplevel options

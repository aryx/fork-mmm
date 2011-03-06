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


open Document
open Viewers

type applet_callback = Widget.widget -> context -> unit

val register : string -> applet_callback -> unit

val error : Widget.widget -> string -> unit

val call : (string, Widget.widget -> Viewers.context -> unit) Hashtbl.t  -> 
           Widget.widget -> Viewers.context -> unit

val get_toplevel_widget : Tk.options list -> Widget.widget

open Tk
open Widget

let version = "$Id: frx_widget.ml,v 1.1 1996/10/22 15:55:38 rouaix Exp $"
(* Make a window (toplevel widget) resizeable *)
let resizeable t =
  update_idletasks(); (* wait until layout is computed *)
  Wm.minsize_set t (Winfo.width t) (Winfo.height t)


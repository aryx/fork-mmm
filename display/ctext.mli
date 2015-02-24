(*s: ./display/ctext.mli *)

(*s: signature Ctext.create *)
(* [create parent opts nav_keys] creates a text widget
 * with "pixel scrolling". Based on a trick learned from Steve Ball.
 * Returns (frame widget, text widget).
 *)
val create :
  Widget.widget -> Tk.options list -> bool -> 
  Widget.widget * Widget.widget
(*e: signature Ctext.create *)

(*s: signature Ctext.init *)
val init : unit -> unit
(*e: signature Ctext.init *)
(*e: ./display/ctext.mli *)

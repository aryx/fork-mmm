val create :
  Widget.widget -> Tk.options list -> bool -> Widget.widget * Widget.widget
  (* [create parent opts nav_keys] creates a text widget
     with "pixel scrolling". Based on a trick learned from Steve Ball.
     Returns (frame widget, text widget).
   *)



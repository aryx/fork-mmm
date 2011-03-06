val new_vertical :
  Widget.widget -> int -> int -> Widget.widget * (int -> unit)
  (* [new_vertical parent width height]
     creates a vertical fillbox of [width] and [height].
     Returns a frame widget and a function to set the current value of
     the fillbox. The value can be
      n < 0        : the fillbox changes color (reddish)
      0 <= n <= 100: the fillbox fills up to n percent
      100 <= n     : the fillbox fills up to 95%
   *)

val new_horizontal :
  Widget.widget -> int -> int -> Widget.widget * (int -> unit)
  (* save as above, except the widget is horizontal *)

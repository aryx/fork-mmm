
val debug : bool ref

val set_initial_width : Widget.widget -> int option
val set_initial_height: Widget.widget -> unit

(* [horiz textw stop continuation] returns [scrollcommand, check] *)
val horiz: 
  Widget.widget -> (unit -> bool) -> (unit -> unit) ->
  (float -> float -> unit) * (unit -> unit)

val vert: 
  Widget.widget -> 
  (float -> float -> unit) * (unit -> unit)

val bound_check : Widget.widget -> int -> (unit -> bool)

val fixed_horiz : Widget.widget -> int -> unit

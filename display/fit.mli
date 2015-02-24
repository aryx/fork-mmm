(*s: ./display/fit.mli *)

(*s: signature Fit.debug *)
val debug : bool ref
(*e: signature Fit.debug *)

(*s: signature Fit.set_initial_width *)
val set_initial_width : Widget.widget -> int option
(*e: signature Fit.set_initial_width *)
(*s: signature Fit.set_initial_height *)
val set_initial_height: Widget.widget -> unit
(*e: signature Fit.set_initial_height *)

(*s: signature Fit.horiz *)
(* [horiz textw stop continuation] returns [scrollcommand, check] *)
val horiz: 
  Widget.widget -> (unit -> bool) -> (unit -> unit) ->
  (float -> float -> unit) * (unit -> unit)
(*e: signature Fit.horiz *)

(*s: signature Fit.vert *)
val vert: 
  Widget.widget -> 
  (float -> float -> unit) * (unit -> unit)
(*e: signature Fit.vert *)

(*s: signature Fit.bound_check *)
val bound_check : Widget.widget -> int -> (unit -> bool)
(*e: signature Fit.bound_check *)

(*s: signature Fit.fixed_horiz *)
val fixed_horiz : Widget.widget -> int -> unit
(*e: signature Fit.fixed_horiz *)
(*e: ./display/fit.mli *)

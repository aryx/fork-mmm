open Widget

val debug: bool ref
val vert: widget -> (float -> float -> unit) * (unit -> unit)

(* [vert widget]
   can be applied to a text widget so that it expands to show its full
   contents. Returns [scroll] and [check]. [scroll] must be used as
   the YScrollCommand of the widget. [check] can be called when some
   modification occurs in the content of the widget (such as a size change
   in some embedded windows.
   This feature is a terrible hack and should be used with extreme caution.
 *)

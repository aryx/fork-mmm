module Widget : sig
  type widget
	  (* widget is an abstract type *)
  val name : widget -> string
	(* Returns the name (tk "path") of a widget *)
  exception IllegalWidgetType of string
end

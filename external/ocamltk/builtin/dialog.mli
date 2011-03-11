val create :
  widget -> string -> string -> bitmap -> int -> string list -> int
  (* [create parent title message bitmap default button_names]
     cf. tk_dialog *)

val create_named :
  widget -> string -> string -> string -> bitmap -> int -> string list -> int
  (* [create_named parent name title message bitmap default button_names]
     cf. tk_dialog *)


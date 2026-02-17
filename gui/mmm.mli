(*s: gui/mmm.mli *)
(*s: signature [[Mmm.user_file]] *)
val user_file : string -> string
    (* [user_file base] returns $HOME/.mmm/[base] *)
(*e: signature [[Mmm.user_file]] *)

(*s: signature [[Mmm.initial_navigator]] *)
val initial_navigator : string -> string option -> unit
(*e: signature [[Mmm.initial_navigator]] *)
(*s: signature [[Mmm.main_navigator]] *)
val main_navigator : Nav.t option ref
(*e: signature [[Mmm.main_navigator]] *)

(*s: signature [[Mmm.helpurl]] *)
(* Preferences, options *)
val helpurl : Url.t ref
(*e: signature [[Mmm.helpurl]] *)
(*s: signature [[Mmm.initial_geom]] *)
val initial_geom : string option ref
(*e: signature [[Mmm.initial_geom]] *)

(*s: signature [[Mmm.add_user_menu]] *)
(* Used for applets *)
val add_user_menu : string -> (Viewers.context -> unit) -> unit
(*e: signature [[Mmm.add_user_menu]] *)
(*s: signature [[Mmm.navigator]] *)
val navigator : bool -> Url.t -> Nav.t option
(*e: signature [[Mmm.navigator]] *)
(*s: signature [[Mmm.new_window_initial]] *)
val new_window_initial : unit -> unit
(*e: signature [[Mmm.new_window_initial]] *)
(*s: signature [[Mmm.new_window_sel]] *)
val new_window_sel : unit -> unit
(*e: signature [[Mmm.new_window_sel]] *)
(*s: signature [[Mmm.change_tachy]] *)
val change_tachy : (Widget.widget -> Low.tachymeter) -> unit
(*e: signature [[Mmm.change_tachy]] *)
(*e: gui/mmm.mli *)

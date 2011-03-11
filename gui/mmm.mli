val user_file : string -> string
    (* [user_file base] returns $HOME/.mmm/[base] *)

val initial_navigator : string -> string option -> Nav.t option
val main_navigator : Nav.t option ref

(* Preferences, options *)
val helpurl : Url.t ref
val initial_geom : string option ref

(* Used for applets *)
val add_user_menu : string -> (Viewers.context -> unit) -> unit
val navigator : bool -> Url.t -> Nav.t option
val new_window_initial : unit -> unit
val new_window_sel : unit -> unit
val change_tachy : (Widget.widget -> Low.tachymeter) -> unit

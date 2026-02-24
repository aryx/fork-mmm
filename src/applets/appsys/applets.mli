
type applet_callback = Widget.widget -> Viewers.context -> unit

val register : string -> applet_callback -> unit

val error : Widget.widget -> string -> unit

val call : (string, Widget.widget -> Viewers.context -> unit) Hashtbl.t  -> 
           Widget.widget -> Viewers.context -> unit

val get_toplevel_widget : Tk.options list -> Widget.widget

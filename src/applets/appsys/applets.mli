(*s: applets.mli *)

(*s: type [[Applets.applet_callback (applets.mli)]] *)
type applet_callback = Widget.widget -> Viewers.context -> unit
(*e: type [[Applets.applet_callback (applets.mli)]] *)

(*s: signature [[Applets.register]] *)
val register : string -> applet_callback -> unit
(*e: signature [[Applets.register]] *)

(*s: signature [[Applets.error]] *)
val error : Widget.widget -> string -> unit
(*e: signature [[Applets.error]] *)

(*s: signature [[Applets.call]] *)
val call : (string, Widget.widget -> Viewers.context -> unit) Hashtbl.t  -> 
           Widget.widget -> Viewers.context -> unit
(*e: signature [[Applets.call]] *)

(*s: signature [[Applets.get_toplevel_widget]] *)
val get_toplevel_widget : Tk.options list -> Widget.widget
(*e: signature [[Applets.get_toplevel_widget]] *)
(*e: applets.mli *)

open Prefs

(* We need a more generic mechanism *)
val plug_applets : (Widget.widget -> pref_family) -> unit

val home : string ref

val f : string -> unit -> unit

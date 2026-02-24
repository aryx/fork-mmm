(*s: gui/mmmprefs.mli *)
open Prefs

(*s: signature [[Mmmprefs.plug_applets]] *)
(* We need a more generic mechanism *)
val plug_applets : (Widget.widget -> pref_family) -> unit
(*e: signature [[Mmmprefs.plug_applets]] *)

(*s: signature [[Mmmprefs.home]] *)
val home : string ref
(*e: signature [[Mmmprefs.home]] *)

(*s: signature [[Mmmprefs.f]] *)
val f : Fpath.t -> unit -> unit
(*e: signature [[Mmmprefs.f]] *)
(*e: gui/mmmprefs.mli *)

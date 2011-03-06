(* Synthetic events *)
open Tk
open Widget


val send : string -> widget -> unit
  (* [send event_name widget] *)

val broadcast : string -> unit
  (* [broadcase event_name] *)

val bind : widget -> string -> (widget -> unit) -> unit
  (* [bind event_name callback] *)

val remove : widget -> string -> unit
  (* [remove widget event_name] *)

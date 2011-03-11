(* The font commands  *)
open Tk
open Widget
open Textvariable
val actual : font -> fontOptions list -> string 
(* tk invocation: font actual <font> <fontOptions list> *)

val actual_displayof : font -> widget -> fontOptions list -> string 
(* tk invocation: font actual <font> -displayof <widget> <fontOptions list> *)

val configure : font -> fontOptions list -> unit 
(* tk invocation: font configure <font> <fontOptions list> *)

val create : fontOptions list -> font 
(* tk invocation: font create <fontOptions list> *)

val delete : font -> unit 
(* tk invocation: font delete <font> *)

val failsafe : string -> unit 
(* tk invocation: font failsafe <string> *)

val families : unit ->string list 
(* tk invocation: font families *)

val families_displayof : widget -> string list 
(* tk invocation: font families -displayof <widget> *)

val measure : font -> string -> int 
(* tk invocation: font measure <font> <string> *)

val measure_displayof : font -> widget -> string -> int 
(* tk invocation: font measure <font> -displayof <widget> <string> *)

val metrics : font -> string -> int 
(* tk invocation: font metrics <font> <string> *)

val metrics_displayof : font -> widget -> string -> int 
(* tk invocation: font metrics <font> -displayof <widget> <string> *)

val metrics_linespace : font -> int 
(* tk invocation: font metrics <font> -linespace *)

val names : unit ->string list 
(* tk invocation: font names *)


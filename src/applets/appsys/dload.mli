(*s: dload.mli *)

(* The type of entry point functions registered by the applet *)
(*s: type [[Dload.applet_callback (dload.mli)]] *)
type applet_callback = Widget.widget -> Viewers.context -> unit
(*e: type [[Dload.applet_callback (dload.mli)]] *)

(* The "foreign" module fake cache *)
(*s: type [[Dload.t (dload.mli)]] *)
type t = {
  module_address : string;		(* the URL of the bytecode *)
  module_info : string list;		(* headers *)
  module_functions : (string, applet_callback) Hashtbl.t
  } 
(*e: type [[Dload.t (dload.mli)]] *)

(*s: type [[Dload.mod_status (dload.mli)]] *)
type mod_status =
    Unavailable of string list
  | Rejected of string list
  | Loaded of t
(*e: type [[Dload.mod_status (dload.mli)]] *)

(*s: signature [[Dload.get]] *)
val get : Url.t -> mod_status
(*e: signature [[Dload.get]] *)
(*s: signature [[Dload.remove]] *)
val remove : Url.t -> unit
(*e: signature [[Dload.remove]] *)
(*s: signature [[Dload.iter]] *)
val iter : (Url.t -> mod_status -> unit) -> unit
(*e: signature [[Dload.iter]] *)

(*s: signature [[Dload.register]] *)
val register : string -> applet_callback -> unit
(*e: signature [[Dload.register]] *)
(*s: signature [[Dload.add_pending_applet]] *)
val add_pending_applet :
  Url.t -> ((string, applet_callback) Hashtbl.t -> unit) -> bool
(*e: signature [[Dload.add_pending_applet]] *)
(*s: signature [[Dload.load]] *)
(* returns true if this is the first applet for this bytecode *)

val load: Document.t  -> unit
(*e: signature [[Dload.load]] *)

(*s: signature [[Dload.load_local]] *)
val load_local : string -> unit
(*e: signature [[Dload.load_local]] *)

(*s: signature [[Dload.paranoid]] *)
val paranoid : bool ref
(*e: signature [[Dload.paranoid]] *)

(*s: signature [[Dload.in_load]] *)
val in_load : bool ref
(*e: signature [[Dload.in_load]] *)
(*e: dload.mli *)

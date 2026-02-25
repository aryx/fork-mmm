(*s: dload.mli *)

(*s: type [[Dload.applet_callback]] *)
(* The type of entry point functions registered by the applet *)
type applet_callback = Widget.widget -> Viewers.context -> unit
(*e: type [[Dload.applet_callback]] *)

(*s: type [[Dload.t]] *)
(* The "foreign" module fake cache : what we keep in memory *)
type t = {
  module_address : string;		(* the URL of the bytecode *)
  module_info : string list;		(* headers *)
  module_functions : (string, applet_callback) Hashtbl.t
  } 
(*e: type [[Dload.t]] *)

(*s: type [[Dload.mod_status]] *)
type mod_status =
  | Loaded of t
  | Unavailable of string list
  | Rejected of string list
(*e: type [[Dload.mod_status]] *)

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
(* returns true if this is the first applet for this bytecode *)
(*e: signature [[Dload.add_pending_applet]] *)

(*s: signature [[Dload.load]] *)
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

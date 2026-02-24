(***********************************************************************)
(*                                                                     *)
(*                           Calves                                    *)
(*                                                                     *)
(*          Francois Rouaix, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* The type of entry point functions registered by the applet *)
type applet_callback = Widget.widget -> Viewers.context -> unit

(* The "foreign" module fake cache *)
type t = {
  module_address : string;		(* the URL of the bytecode *)
  module_info : string list;		(* headers *)
  module_functions : (string, applet_callback) Hashtbl.t
  } 

type mod_status =
    Unavailable of string list
  | Rejected of string list
  | Loaded of t

val get : Url.t -> mod_status
val remove : Url.t -> unit
val iter : (Url.t -> mod_status -> unit) -> unit

val register : string -> applet_callback -> unit
val add_pending_applet :
  Url.t -> ((string, applet_callback) Hashtbl.t -> unit) -> bool
(* returns true if this is the first applet for this bytecode *)

val load: Document.t  -> unit

val load_local : string -> unit

val paranoid : bool ref

val in_load : bool ref

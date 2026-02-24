(*s: capabilities.mli *)
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

(* The rights as stored *)
(*s: type [[Capabilities.right (capabilities.mli)]] *)
type right =
   FileR of string			(* read access to files *)
 | FileW of string			(* write acces to files *)
 | DocumentR of string			(* read access to URLs *)
    (* Document read access affects decoders, embedded viewers, as well as
       the general retrieval mechanism *)
 | HTMLDisplay
   (* HTML display machine access *)    
 | Internals
(*e: type [[Capabilities.right (capabilities.mli)]] *)

module Rights : Set.S with type elt = bool * right

(*s: type [[Capabilities.mode (capabilities.mli)]] *)
type mode = Fixed | Extend | Temporary
(*e: type [[Capabilities.mode (capabilities.mli)]] *)

(*s: type [[Capabilities.t (capabilities.mli)]] *)
type t = {
  mutable mode : mode;
  mutable rights : Rights.t;
  who : Url.t;
  }
(*e: type [[Capabilities.t (capabilities.mli)]] *)

(*s: signature [[Capabilities.local_default]] *)
val local_default : Url.t -> t
(*e: signature [[Capabilities.local_default]] *)
(*s: signature [[Capabilities.lenient_default]] *)
val lenient_default : Url.t -> t
(*e: signature [[Capabilities.lenient_default]] *)
(*s: signature [[Capabilities.strict_default]] *)
val strict_default : Url.t -> t
(*e: signature [[Capabilities.strict_default]] *)
(*s: signature [[Capabilities.lenient_default (capabilities.mli)]] *)
val lenient_default : Url.t -> t
(*e: signature [[Capabilities.lenient_default (capabilities.mli)]] *)


(*s: signature [[Capabilities.set]] *)
val set : t -> unit
(*e: signature [[Capabilities.set]] *)
(*s: signature [[Capabilities.reset]] *)
val reset : unit -> unit
(*e: signature [[Capabilities.reset]] *)

(*s: signature [[Capabilities.ask]] *)
val ask: t -> right -> bool
(*e: signature [[Capabilities.ask]] *)
  (* [ask capa right] *)
  (* REMEMBER TO MAKE COPIES OF ARGUMENT IF MUTABLE (eg string) *)


(*s: exception [[Capabilities.Denied (capabilities.mli)]] *)
exception Denied
(*e: exception [[Capabilities.Denied (capabilities.mli)]] *)

(*s: signature [[Capabilities.require]] *)
val require: t -> right list -> bool
(*e: signature [[Capabilities.require]] *)
  (* get some specific capabilities, to avoid popping dialog boxes all
     over the place. Moreover, can make use of regexp
   *)

(*s: signature [[Capabilities.get]] *)
(* IMPORTANT: The following functions may only be called at load-time *)

val get : unit -> t 
(*e: signature [[Capabilities.get]] *)
  (* get the default capabilities *)

(*e: capabilities.mli *)

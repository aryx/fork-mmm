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
type right =
   FileR of string			(* read access to files *)
 | FileW of string			(* write acces to files *)
 | DocumentR of string			(* read access to URLs *)
    (* Document read access affects decoders, embedded viewers, as well as
       the general retrieval mechanism *)
 | HTMLDisplay
   (* HTML display machine access *)    
 | Internals

module Rights : Set.S with type elt = bool * right

type mode = Fixed | Extend | Temporary

type t = {
  mutable mode : mode;
  mutable rights : Rights.t;
  who : Url.t;
  }

val local_default : Url.t -> t
val lenient_default : Url.t -> t
val strict_default : Url.t -> t
val lenient_default : Url.t -> t


val set : t -> unit
val reset : unit -> unit

val ask: t -> right -> bool
  (* [ask capa right] *)
  (* REMEMBER TO MAKE COPIES OF ARGUMENT IF MUTABLE (eg string) *)


exception Denied

val require: t -> right list -> bool
  (* get some specific capabilities, to avoid popping dialog boxes all
     over the place. Moreover, can make use of regexp
   *)

(* IMPORTANT: The following functions may only be called at load-time *)

val get : unit -> t 
  (* get the default capabilities *)


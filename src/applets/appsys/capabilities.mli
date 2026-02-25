(*s: capabilities.mli *)

(*s: type [[Capabilities.right]] *)
(* The rights as stored *)
type right =
   FileR of string			(* read access to files *)
 | FileW of string			(* write acces to files *)
 | DocumentR of string			(* read access to URLs *)
    (* Document read access affects decoders, embedded viewers, as well as
       the general retrieval mechanism. It means that the applet has
       access to the document body. (Navigation, that is triggering 
       retrieval/display of documents is always available, since inspection
       of URL is essentially useless).
     *)
 | HTMLDisplay
   (* HTML display machine access : this is very liberal; if granted, it
      means that the applet has access to *all* retrieved HTML documents
    *)
 | Internals
(*e: type [[Capabilities.right]] *)

module Rights : Set.S with type elt = bool * right

(*s: type [[Capabilities.mode]] *)
type mode = Fixed | Extend | Temporary (* extension mode for access rights *)
(*e: type [[Capabilities.mode]] *)

(*s: type [[Capabilities.t]] *)
type t = {
  mutable mode : mode;
  mutable rights : Rights.t;
  who: Url.t; (* where this applet was loaded from. *)
  }
(*e: type [[Capabilities.t]] *)

(*s: signature [[Capabilities.local_default]] *)
val local_default : Url.t -> t
(*e: signature [[Capabilities.local_default]] *)
(*s: signature [[Capabilities.lenient_default]] *)
val lenient_default : Url.t -> t
(*e: signature [[Capabilities.lenient_default]] *)
(*s: signature [[Capabilities.strict_default]] *)
val strict_default : Url.t -> t
(*e: signature [[Capabilities.strict_default]] *)


(*s: signature [[Capabilities.set]] *)
val set : t -> unit
(*e: signature [[Capabilities.set]] *)
(*s: signature [[Capabilities.reset]] *)
val reset : unit -> unit
(*e: signature [[Capabilities.reset]] *)

(*s: signature [[Capabilities.ask]] *)
val ask: t -> right -> bool
  (* [ask capa right] *)
  (* REMEMBER TO MAKE COPIES OF ARGUMENT IF MUTABLE (eg string) *)
(*e: signature [[Capabilities.ask]] *)


(*s: exception [[Capabilities.Denied]] *)
exception Denied (* access denied *)
(*e: exception [[Capabilities.Denied]] *)

(*s: signature [[Capabilities.require]] *)
val require: t -> right list -> bool
  (* get some specific capabilities, to avoid popping dialog boxes all
     over the place. Moreover, can make use of regexp
   *)
(*e: signature [[Capabilities.require]] *)

(* IMPORTANT: The following functions may only be called at load-time *)

(*s: signature [[Capabilities.get]] *)
val get : unit -> t 
  (* get the default capabilities *)
(*e: signature [[Capabilities.get]] *)
(*e: capabilities.mli *)

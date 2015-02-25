(*s: ./commons/ebuffer.mli *)
(*s: copyright header v6 *)
(***********************************************************************)
(*                                                                     *)
(*                           The V6 Engine                             *)
(*                                                                     *)
(*          Francois Rouaix, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header v6 *)

(* Extensible buffers *)

type t 

(*s: signature Ebuffer.create *)
val create : int -> t
  (* [create n] creates a buffer with initial size [n] *)
(*e: signature Ebuffer.create *)

(*s: signature Ebuffer.output_string *)
val output_string : t -> string -> unit
  (* [output_string buf s] appends [s] to [buf] *)
(*e: signature Ebuffer.output_string *)
(*s: signature Ebuffer.output_char *)
val output_char : t -> char -> unit
  (* [output_char buf c] appends [c] to [buf] *)
(*e: signature Ebuffer.output_char *)
(*s: signature Ebuffer.output *)
val output : t -> string -> int -> int -> unit
  (* [output buf s offs len] appends [len] characters of [s], starting
     at offset [offs] to [buf].
     Raises [Invalid_argument] if [offs] and [len] do not designate a
     valid substring of [s] *)
(*e: signature Ebuffer.output *)

(*s: signature Ebuffer.get *)
val get : t -> string
  (* [get buf] returns the current contents of [buf] *)
(*e: signature Ebuffer.get *)
(*s: signature Ebuffer.used *)
val used : t -> int
  (* [used buf] returns the current length of [buf] *)
(*e: signature Ebuffer.used *)

(*s: signature Ebuffer.reset *)
val reset : t -> unit
  (* [reset buf] emties [buf] *)  
(*e: signature Ebuffer.reset *)
(*e: ./commons/ebuffer.mli *)

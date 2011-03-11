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

(* $Id: ebuffer.mli,v 1.1 1996/10/22 13:12:42 rouaix Exp $ *)

(* Extensible buffers *)

type t 

val create : int -> t
  (* [create n] creates a buffer with initial size [n] *)

val output_string : t -> string -> unit
  (* [output_string buf s] appends [s] to [buf] *)
val output_char : t -> char -> unit
  (* [output_char buf c] appends [c] to [buf] *)
val output : t -> string -> int -> int -> unit
  (* [output buf s offs len] appends [len] characters of [s], starting
     at offset [offs] to [buf].
     Raises [Invalid_argument] if [offs] and [len] do not designate a
     valid substring of [s] *)

val get : t -> string
  (* [get buf] returns the current contents of [buf] *)
val used : t -> int
  (* [used buf] returns the current length of [buf] *)

val reset : t -> unit
  (* [reset buf] emties [buf] *)  

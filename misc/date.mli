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

(* $Id: date.mli,v 1.3 1998/12/08 15:28:07 furuse Exp $ *)

val asc_wkday : int -> string
    (* [asc_wkday n] maps 0..6 to Sun..Sat *)

val asc_month : int -> string
    (* [asc_month n] maps 0..11 to Jan..Dec *)

val asc : float -> string
    (* [asc uxtime] RFC822 of unix time *)

val asc_now : unit -> string
    (* [asc_now ()] RFC822 of now *)

val commonlog : float -> string
  (* Text version (Common log format) of an Unix time value *)

val compare_time : int list * int list -> int
    (* [compare_time l1 l2] compare lists encodings of timestamps
       Encoding must be:
        [year; month; mday; hour; min; sec]
     *)


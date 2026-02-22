(*s: commons/date.mli *)
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

(*s: signature [[Date.asc_wkday]] *)
val asc_wkday : int -> string
    (* [asc_wkday n] maps 0..6 to Sun..Sat *)
(*e: signature [[Date.asc_wkday]] *)
(*s: signature [[Date.asc_month]] *)
val asc_month : int -> string
    (* [asc_month n] maps 0..11 to Jan..Dec *)
(*e: signature [[Date.asc_month]] *)
(*s: signature [[Date.asc]] *)
val asc : float -> string
    (* [asc uxtime] RFC822 of unix time *)
(*e: signature [[Date.asc]] *)
(*s: signature [[Date.asc_now]] *)
val asc_now : unit -> string
    (* [asc_now ()] RFC822 of now *)
(*e: signature [[Date.asc_now]] *)

(*s: signature [[Date.commonlog]] *)
val commonlog : float -> string
  (* Text version (Common log format) of an Unix time value *)
(*e: signature [[Date.commonlog]] *)

(*s: signature [[Date.compare_time]] *)
val compare_time : int list * int list -> int
    (* [compare_time l1 l2] compare lists encodings of timestamps
       Encoding must be:
        [year; month; mday; hour; min; sec]
     *)
(*e: signature [[Date.compare_time]] *)
(*e: commons/date.mli *)

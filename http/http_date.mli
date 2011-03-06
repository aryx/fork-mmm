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

(* $Id: http_date.mli,v 1.2 1998/12/08 15:28:33 furuse Exp $ *)

(* HTTP Date format *)

(* Based on Unix.tm *)
type http_time =
  { ht_sec : int;                       (* Seconds 0..59 *)
    ht_min : int;                       (* Minutes 0..59 *)
    ht_hour : int;                      (* Hours 0..23 *)
    ht_mday : int;                      (* Day of month 1..31 *)
    ht_mon : int;                       (* Month of year 0..11 *)
    ht_year : int;                      (* Year - 1900 *)
    ht_wday : int }                     (* Day of week (Sunday is 0) *)


val expired : http_time -> bool
  (* Determines if an http_time is in the past *)

val compare : http_time -> http_time -> int
  (* Compares two http_times *)

val string_of_ht : http_time -> string
  (* Text version (RFC822) of an http time stamp *)

val tm_of_ht : http_time -> Unix.tm
val stamp_of_ht : http_time -> float

val ht_of_stamp : float -> http_time

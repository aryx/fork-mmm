(*s: http/http_date.mli *)
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

(* HTTP Date format *)

(*s: type [[Http_date.http_time]] *)
(* Based on Unix.tm *)
type http_time =
  { ht_sec : int;                       (* Seconds 0..59 *)
    ht_min : int;                       (* Minutes 0..59 *)
    ht_hour : int;                      (* Hours 0..23 *)
    ht_mday : int;                      (* Day of month 1..31 *)
    ht_mon : int;                       (* Month of year 0..11 *)
    ht_year : int;                      (* Year - 1900 *)
    ht_wday : int }                     (* Day of week (Sunday is 0) *)
(*e: type [[Http_date.http_time]] *)


(*s: signature [[Http_date.expired]] *)
val expired : http_time -> bool
  (* Determines if an http_time is in the past *)
(*e: signature [[Http_date.expired]] *)

(*s: signature [[Http_date.compare]] *)
val compare : http_time -> http_time -> int
  (* Compares two http_times *)
(*e: signature [[Http_date.compare]] *)

(*s: signature [[Http_date.string_of_ht]] *)
val string_of_ht : http_time -> string
  (* Text version (RFC822) of an http time stamp *)
(*e: signature [[Http_date.string_of_ht]] *)

(*s: signature [[Http_date.tm_of_ht]] *)
val tm_of_ht : http_time -> Unix.tm
(*e: signature [[Http_date.tm_of_ht]] *)
(*s: signature [[Http_date.stamp_of_ht]] *)
val stamp_of_ht : http_time -> float
(*e: signature [[Http_date.stamp_of_ht]] *)

(*s: signature [[Http_date.ht_of_stamp]] *)
val ht_of_stamp : float -> http_time
(*e: signature [[Http_date.ht_of_stamp]] *)
(*e: http/http_date.mli *)

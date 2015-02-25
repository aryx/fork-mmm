(*s: ./http/http_date.ml *)
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

open Printf
open Unix
open Mstring
open Date

(*s: type Http_date.http_time (./http/http_date.ml) *)
(* Based on Unix.tm *)
type http_time =
  { ht_sec : int;                       (* Seconds 0..59 *)
    ht_min : int;                       (* Minutes 0..59 *)
    ht_hour : int;                      (* Hours 0..23 *)
    ht_mday : int;                      (* Day of month 1..31 *)
    ht_mon : int;                       (* Month of year 0..11 *)
    ht_year : int;                      (* Year - 1900 *)
    ht_wday : int }                     (* Day of week (Sunday is 0) *)
(*e: type Http_date.http_time (./http/http_date.ml) *)

(*s: function Http_date.expired *)
let expired ht =
  let now = gmtime(time()) in
  let lht = 
    [ht.ht_year; ht.ht_mon; ht.ht_mday; ht.ht_hour; ht.ht_min; ht.ht_sec]
  and lnow =
    [now.tm_year; now.tm_mon; now.tm_mday; now.tm_hour; now.tm_min; now.tm_sec]
  in
    compare_time (lht, lnow) <= 0
(*e: function Http_date.expired *)

(*s: function Http_date.compare *)
let compare ht1 ht2 =
 compare_time
  ([ht1.ht_year; ht1.ht_mon; ht1.ht_mday; ht1.ht_hour; ht1.ht_min; ht1.ht_sec],
   [ht2.ht_year; ht2.ht_mon; ht2.ht_mday; ht2.ht_hour; ht2.ht_min; ht2.ht_sec])
(*e: function Http_date.compare *)

(*s: function Http_date.string_of_ht *)
let string_of_ht ht =
  sprintf "%s, %02d %s %d %02d:%02d:%02d GMT"
      (asc_wkday ht.ht_wday)
      ht.ht_mday
      (asc_month ht.ht_mon)
      (ht.ht_year + 1900)
      ht.ht_hour
      ht.ht_min
      ht.ht_sec
(*e: function Http_date.string_of_ht *)

(*s: function Http_date.tm_of_ht *)
(* 
let has_dst = localtime(time()).tm_isdst
*)
let tm_of_ht ht = {
    tm_sec = ht.ht_sec;
    tm_min = ht.ht_min;
    tm_hour = ht.ht_hour;
    tm_mday = ht.ht_mday;
    tm_mon = ht.ht_mon;
    tm_year = ht.ht_year;
    tm_wday = ht.ht_wday;
    tm_yday = 0;
    tm_isdst = false        (* I don't have a clue here *)
   }
(*e: function Http_date.tm_of_ht *)

(*s: function Http_date.stamp_of_ht *)
let stamp_of_ht ht =
   fst (mktime (tm_of_ht ht))
(*e: function Http_date.stamp_of_ht *)


(*s: function Http_date.ht_of_stamp *)
let ht_of_stamp ut =
  let tm = gmtime ut in {
    ht_sec = tm.tm_sec;
    ht_min = tm.tm_min;
    ht_hour = tm.tm_hour;
    ht_mday = tm.tm_mday;
    ht_mon = tm.tm_mon;
    ht_year = tm.tm_year;
    ht_wday = tm.tm_wday
     }
(*e: function Http_date.ht_of_stamp *)
(*e: ./http/http_date.ml *)

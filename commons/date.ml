(*s: commons/date.ml *)
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

(*s: function [[Date.asc_wkday]] *)
let asc_wkday = function
   0 -> "Sun"
 | 1 -> "Mon"
 | 2 -> "Tue"
 | 3 -> "Wed"
 | 4 -> "Thu"
 | 5 -> "Fri"
 | 6 -> "Sat"
 | _ -> assert false
(*e: function [[Date.asc_wkday]] *)

(*s: function [[Date.asc_month]] *)
let asc_month = function
   0 -> "Jan"
 | 1 -> "Feb"
 | 2 -> "Mar"
 | 3 -> "Apr"
 | 4 -> "May"
 | 5 -> "Jun"
 | 6 -> "Jul"
 | 7 -> "Aug"
 | 8 -> "Sep"
 | 9 -> "Oct"
 | 10 -> "Nov"
 | 11 -> "Dec"
 | _ -> assert false
(*e: function [[Date.asc_month]] *)

(*s: function [[Date.asc]] *)
(* Produces RFC822 style *)
let asc ut =
  let tm = gmtime ut in
    sprintf "%s, %02d %s %d %02d:%02d:%02d GMT"
        (asc_wkday tm.tm_wday)
    tm.tm_mday
    (asc_month tm.tm_mon)
    (tm.tm_year + 1900)
    tm.tm_hour
    tm.tm_min
    tm.tm_sec
(*e: function [[Date.asc]] *)

(*s: function [[Date.asc_now]] *)
let asc_now () = asc (time())
(*e: function [[Date.asc_now]] *)


(*s: function [[Date.commonlog]] *)
(* Timezone ??? *)
let commonlog int =
  let tm = localtime int in
  sprintf "%02d/%s/%d:%02d:%02d:%02d"
      tm.tm_mday
      (asc_month tm.tm_mon)
      (tm.tm_year + 1900)
      tm.tm_hour
      tm.tm_min
      tm.tm_sec
(*e: function [[Date.commonlog]] *)


(*s: function [[Date.compare_time]] *)
let rec compare_time = function
   [], [] -> 0
 | (x::xx), (y::yy) when x = y -> compare_time (xx, yy)
 | (x::_), (y::_) when x < y -> -1
 | (x::_), (y::_) when x > y -> 1
 |  _, _ -> assert false
(*e: function [[Date.compare_time]] *)
(*e: commons/date.ml *)

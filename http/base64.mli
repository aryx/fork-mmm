(*s: ./http/base64.mli *)
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

(* $Id: base64.mli,v 1.1 1996/10/22 13:12:31 rouaix Exp $ *)

(*s: signature Base64.encode *)
(* Base64 encoding (ONLY for Basic authentication) *)
val encode : string -> string
(*e: signature Base64.encode *)
(*s: signature Base64.decode *)
val decode : string -> string
(*e: signature Base64.decode *)
(*e: ./http/base64.mli *)

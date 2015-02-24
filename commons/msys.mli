(*s: ./commons/msys.mli *)
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

(*s: signature Msys.tilde_subst *)
(* $Id: msys.mli,v 1.1 1996/10/22 13:12:52 rouaix Exp $ *)

val tilde_subst : string -> string
    (* substitute ~ at beginning of file path *)
(*e: signature Msys.tilde_subst *)

(*s: signature Msys.rm *)
val rm: string -> unit
    (* quiet unlink *)
(*e: signature Msys.rm *)

(*s: signature Msys.fsize *)
val fsize: string -> int
    (* file size *)
(*e: signature Msys.fsize *)

(*s: signature Msys.mktemp *)
val mktemp : string -> string
(*e: signature Msys.mktemp *)
(*e: ./commons/msys.mli *)

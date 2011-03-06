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

(* $Id: msys.mli,v 1.1 1996/10/22 13:12:52 rouaix Exp $ *)

val tilde_subst : string -> string
    (* substitute ~ at beginning of file path *)

val rm: string -> unit
    (* quiet unlink *)

val fsize: string -> int
    (* file size *)

val mktemp : string -> string

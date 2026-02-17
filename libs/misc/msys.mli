(*s: commons/msys.mli *)
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

(*s: signature [[Msys.tilde_subst]] *)
val tilde_subst : string -> string
    (* substitute ~ at beginning of file path *)
(*e: signature [[Msys.tilde_subst]] *)

(*s: signature [[Msys.rm]] *)
val rm: string -> unit
    (* quiet unlink *)
(*e: signature [[Msys.rm]] *)

(*s: signature [[Msys.fsize]] *)
val fsize: string -> int
    (* file size *)
(*e: signature [[Msys.fsize]] *)

(*s: signature [[Msys.mktemp]] *)
val mktemp : string -> string
(*e: signature [[Msys.mktemp]] *)
(*e: commons/msys.mli *)

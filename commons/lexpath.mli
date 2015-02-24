(*s: ./commons/lexpath.mli *)
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

(* $Id: lexpath.mli,v 1.1 1996/10/22 13:12:46 rouaix Exp $ *)

(*s: signature Lexpath.path_components *)
(* Path lexer
   Deals with "./" and "../"
 *)

val path_components : Lexing.lexbuf -> string list
  (* in normal order: /a/b/c/ -> [a;b;c] *)
(*e: signature Lexpath.path_components *)

(*s: signature Lexpath.rev_path_components *)
val rev_path_components : Lexing.lexbuf -> string list
  (* in reverse order: /a/b/c/ -> [c;b;a] *)
(*e: signature Lexpath.rev_path_components *)

(*s: signature Lexpath.build *)
val build : string -> string list -> string
  (* [build root [a;b;d]] returns path/a/b/c *)
(*e: signature Lexpath.build *)

(*s: signature Lexpath.remove_dots *)
val remove_dots : string -> string
  (* takes path and removes . and .. *)
(*e: signature Lexpath.remove_dots *)
(*e: ./commons/lexpath.mli *)

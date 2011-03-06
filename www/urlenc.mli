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

(* $Id: urlenc.mli,v 1.3 1997/10/15 13:45:07 rouaix Exp $ *)

(* URL encoding *)

val decode : string -> string
val encode : string -> string
    (* encoding and decoding for an arbitrary string *)

val strict_form_standard : bool ref
    (* if true, we take RFC1866 8.2.1 case 1 strictly, and encode any 
       non-alphanumeric character in the field name
       else, we encode only values, but not field names *)
val form_encode : (string * string) list -> string
val form_decode : string -> (string * string) list
    (* application/x-www-form-urlencoded encoding *)

(*-*)
val unquote : string -> string

(*s: ./www/urlenc.mli *)
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

(*s: signature Urlenc.decode *)
(* URL encoding *)

val decode : string -> string
(*e: signature Urlenc.decode *)
(*s: signature Urlenc.encode *)
val encode : string -> string
    (* encoding and decoding for an arbitrary string *)
(*e: signature Urlenc.encode *)

(*s: signature Urlenc.strict_form_standard *)
val strict_form_standard : bool ref
    (* if true, we take RFC1866 8.2.1 case 1 strictly, and encode any 
       non-alphanumeric character in the field name
       else, we encode only values, but not field names *)
(*e: signature Urlenc.strict_form_standard *)
(*s: signature Urlenc.form_encode *)
val form_encode : (string * string) list -> string
(*e: signature Urlenc.form_encode *)
(*s: signature Urlenc.form_decode *)
val form_decode : string -> (string * string) list
    (* application/x-www-form-urlencoded encoding *)
(*e: signature Urlenc.form_decode *)

(*s: signature Urlenc.unquote *)
(*-*)
val unquote : string -> string
(*e: signature Urlenc.unquote *)
(*e: ./www/urlenc.mli *)

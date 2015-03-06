(*s: ./www/uri.ml *)
open Printf
open Mstring

(*s: type Uri.abs_uri *)
(* URI utilities. RFC 1630 *)

type abs_uri = {
   uri_url : string;
   uri_fragment : string option
 }
(*e: type Uri.abs_uri *)

(*s: function Uri.is_absolute *)
(* RFC 1630, partial forms *)
let is_absolute uri =
  try
    let colonpos = String.index uri ':' in
    try 
      let slashpos = String.index uri '/' in
      colonpos < slashpos (* colon must occur before slash *)
    with
      Not_found -> true (* colon occurs before slash *)
  with
    Not_found -> false (* absolute must have a : *)
(*e: function Uri.is_absolute *)
(*e: ./www/uri.ml *)

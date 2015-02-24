(*s: ./www/uri.ml *)
open Printf
open Mstring

(*s: enum Uri.abs_uri (./www/uri.ml) *)
type abs_uri = {
   uri_url : string;
   uri_frag : string option
 }
(*e: enum Uri.abs_uri (./www/uri.ml) *)

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

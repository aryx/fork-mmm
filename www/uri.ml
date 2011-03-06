open Printf
open Mstring

type abs_uri = {
   uri_url : string;
   uri_frag : string option
 }

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

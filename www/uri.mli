(*s: www/uri.mli *)
(*s: type [[Uri.abs_uri]] *)
(* URI utilities. RFC 1630 *)
type abs_uri = {
   uri_url : string;
   uri_fragment : string option
 }
(*e: type [[Uri.abs_uri]] *)

(*s: signature [[Uri.is_absolute]] *)
val is_absolute : string -> bool
   (* [is_absolute uri] determines if [uri] is absolute according to
      rules of RFC 1630 *)
(*e: signature [[Uri.is_absolute]] *)
(*e: www/uri.mli *)

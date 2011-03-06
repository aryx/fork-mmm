(* URI utilities. RFC 1630 *)

type abs_uri = {
   uri_url : string;
   uri_frag : string option
 }

val is_absolute : string -> bool
   (* [is_absolute uri] determines if [uri] is absolute according to
      rules of RFC 1630 *)




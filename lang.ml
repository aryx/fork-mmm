(* Version *)

let japan = ref false

let lang () =
  if !japan then "ja"
  else "iso8859"

(* detect and set LANG information *)
(* mmm uses this function and Jtk.is_japanse_mode also. *)
let is_japanese () =
  let lang = try Sys.getenv "LANG" with _ -> "" in
  String.length lang >= 2 && String.sub lang 0 2 = "ja"


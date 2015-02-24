(*s: ./commons/lang.ml *)
(*s: constant Lang.japan *)
(* Version *)

let japan = ref false
(*e: constant Lang.japan *)

(*s: function Lang.lang *)
let lang () =
  if !japan then "ja"
  else "iso8859"
(*e: function Lang.lang *)

(* detect and set LANG information *)
(*s: function Lang.is_japanese *)
(* mmm uses this function and Jtk.is_japanse_mode also. *)
let is_japanese () =
  let lang = try Sys.getenv "LANG" with _ -> "" in
  String.length lang >= 2 && String.sub lang 0 2 = "ja"
(*e: function Lang.is_japanese *)

(*e: ./commons/lang.ml *)

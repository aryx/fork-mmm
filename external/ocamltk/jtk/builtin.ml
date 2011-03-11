(* Builtin function *)

(* Autodetection of Japanese mode or default (Latin) mode *)
let is_japanese_mode () =
  if tkEval [| TkToken "info"; TkToken "commands"; TkToken "kanji" |] = ""
  then false (* not with Japanesation *)
  else begin
    (* Now see the environmental variable "LANG" *)
    try
      if Sys.getenv "LANG" = "C" then false else true
    with
    | Not_found ->
      (* We assume Japanese mode. *)
	true
  end

(*
val pref_init : Textvariable.textVariable -> unit
val pref_set  : Textvariable.textVariable -> unit
*)
open File

let r = Str.regexp ":"

let pref_init v =
  Textvariable.set v (String.concat ":" !binary_path)

let pref_set v = 
  binary_path :=
     List.map Msys.tilde_subst (Str.split r (Textvariable.get v))

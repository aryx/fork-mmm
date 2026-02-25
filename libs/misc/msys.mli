(*s: commons/msys.mli *)

(*s: signature [[Msys.tilde_subst]] *)
val tilde_subst : string -> string
    (* substitute ~ at beginning of file path *)
(*e: signature [[Msys.tilde_subst]] *)

(*s: signature [[Msys.rm]] *)
val rm: string -> unit
    (* quiet unlink *)
(*e: signature [[Msys.rm]] *)

(*s: signature [[Msys.fsize]] *)
val fsize: string -> int
    (* file size *)
(*e: signature [[Msys.fsize]] *)

(*s: signature [[Msys.mktemp]] *)
val mktemp : string -> string
(*e: signature [[Msys.mktemp]] *)
(*e: commons/msys.mli *)

(* Builtin function *)

type font = string

let cCAMLtoTKfont s = TkToken s

let cTKtoCAMLfont s = s

open Printf

let debug_mode = ref false

(* flushes ! *)
let f s = try prerr_endline s with _ -> ()

let debug s = if !debug_mode then f s 


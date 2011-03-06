(* Version and other builtin strings *)
val number : int
val http : string              (* the version in User-Agent field *)
val about : string -> string   (* dialog *)
val initurl : string -> string (* fake initial url *)
val helpurl : string -> string (* help url *)
val html : string -> string    (* fake initial document *)
val home : string -> string    (* MMM home page *)
(*-*)
val applet_init : (bool -> unit) ref

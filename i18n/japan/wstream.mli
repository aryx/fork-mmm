open Wchar

type t 

val create : int -> t
  (* [create n] creates a buffer with initial size [n] *)

val output_array : t -> wchar array -> unit
val output_one : t -> wchar -> unit
val output : t -> wchar array -> int -> int -> unit
val get : t -> wchar array
val used : t -> int
val reset : t -> unit
val input : t -> wchar array -> int -> int -> int

(* just for me *)
val hd : t -> wchar
val skip1 : t -> unit

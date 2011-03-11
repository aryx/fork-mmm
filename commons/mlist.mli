(* List utilities *)
val hdn : 'a list -> int -> 'a list
   (* [hdn [a1;a2;...;an;...; ak] returns [a1;a2;...;an] *)
val tln : 'a list -> int -> 'a list
   (* [tln [a1;a2;...;an;...; ak] returns [an+1;...; ak] *)

val except_assoc: 'a -> ('a * 'b) list -> ('a * 'b) list
val exceptq: 'a -> 'a list -> 'a list
val rev_do_list : ('a -> unit) -> 'a list -> unit

val do_listi : (int -> 'a -> unit) -> int -> 'a list -> unit

(*s: commons/mlist.mli *)
(*s: signature [[Mlist.hdn]] *)
(* List utilities *)
val hdn : 'a list -> int -> 'a list
   (* [hdn [a1;a2;...;an;...; ak] returns [a1;a2;...;an] *)
(*e: signature [[Mlist.hdn]] *)
(*s: signature [[Mlist.tln]] *)
val tln : 'a list -> int -> 'a list
   (* [tln [a1;a2;...;an;...; ak] returns [an+1;...; ak] *)
(*e: signature [[Mlist.tln]] *)

(*s: signature [[Mlist.except_assoc]] *)
val except_assoc: 'a -> ('a * 'b) list -> ('a * 'b) list
(*e: signature [[Mlist.except_assoc]] *)
(*s: signature [[Mlist.exceptq]] *)
val exceptq: 'a -> 'a list -> 'a list
(*e: signature [[Mlist.exceptq]] *)
(*s: signature [[Mlist.rev_do_list]] *)
val rev_do_list : ('a -> unit) -> 'a list -> unit
(*e: signature [[Mlist.rev_do_list]] *)

(*s: signature [[Mlist.do_listi]] *)
val do_listi : (int -> 'a -> unit) -> int -> 'a list -> unit
(*e: signature [[Mlist.do_listi]] *)
(*e: commons/mlist.mli *)

(*s: ./commons/error.mli *)

(*s: signature class Error.t *)
class virtual t : object
 method virtual f : string -> unit
 method virtual ok : string -> unit
 method virtual choose : string -> bool
 method virtual ari : string -> int
end
(*e: signature class Error.t *)

(*s: signature Error.default *)
val default : t ref
(*e: signature Error.default *)

(*s: signature Error.f *)
val f : string -> unit
(*e: signature Error.f *)
(*s: signature Error.ok *)
val ok : string -> unit
(*e: signature Error.ok *)
(*s: signature Error.choose *)
val choose : string -> bool
(*e: signature Error.choose *)
(*s: signature Error.ari *)
val ari : string -> int
(*e: signature Error.ari *)

(*e: ./commons/error.mli *)

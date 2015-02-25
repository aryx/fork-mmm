(*s: ./commons/error.mli *)

(*s: signature class Error.t *)
class t : (Widget.widget) -> object
 method f : string -> unit
 method ok : string -> unit
 method choose : string -> bool
 method ari : string -> int
end
(*e: signature class Error.t *)

(*s: signature Error.default *)
val default : t
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

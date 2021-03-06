(*s: ./http/auth.mli *)

(*s: type Auth.authSpace *)
(* Authorizations are remembered on the base of the directory url and realm
 * They are kept during the whole MMM session, with expiration
 *)
type authSpace = {
   auth_proxy: bool;
   auth_host : string;
   auth_port : int;
   auth_dir : string;
   auth_realm : string
  }
(*e: type Auth.authSpace *)


(*s: signature Auth.lifetime *)
val lifetime : int ref
(*e: signature Auth.lifetime *)
(*s: signature Auth.auth_file *)
val auth_file : string ref
(*e: signature Auth.auth_file *)

val edit_backend: (unit -> unit) ref

(* pad: only for edit_backend *)
type authEntry = {
   auth_cookie : string;
   mutable auth_lastused : float
   }
val authorizations: (authSpace, authEntry) Hashtbl.t

(*s: signature Auth.edit *)
val edit : unit -> unit
(*e: signature Auth.edit *)
(*s: signature Auth.load *)
val load : unit -> unit
(*e: signature Auth.load *)
(*s: signature Auth.save *)
val save : unit -> unit
(*e: signature Auth.save *)

(*s: signature Auth.add *)
val add : authSpace -> string -> unit
(*e: signature Auth.add *)
(*s: signature Auth.get *)
val get : authSpace -> string
(*e: signature Auth.get *)

(*s: signature Auth.init *)
val init : unit -> unit
(*e: signature Auth.init *)

val open_passwd_ref: (string -> string * string) ref

(*s: signature Auth.check *)
val check : Www.request -> Http_headers.authChallenge -> authSpace ->
                  (string * bool * authSpace) option
(*e: signature Auth.check *)
(*e: ./http/auth.mli *)

open Http_headers

type authSpace = {
   auth_proxy: bool;
   auth_host : string;
   auth_port : int;
   auth_dir : string;
   auth_realm : string
  }


val lifetime : int ref
val auth_file : string ref

val edit : unit -> unit
val load : unit -> unit
val save : unit -> unit

val add : authSpace -> string -> unit
val get : authSpace -> string

val init : unit -> unit

val check : Www.request -> authChallenge -> authSpace ->
                  (string * bool * authSpace) option

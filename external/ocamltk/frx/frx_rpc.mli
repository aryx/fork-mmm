(* Some notion of RPC *)

val register : string -> (string list -> unit) -> unit
  (* [register external_name f] *)
val invoke : string -> string -> string list -> string
  (* [invoke interp name args] *)
val async_invoke : string -> string -> string list -> unit
  (* [async_invoke interp name args] *)
val rpc_info : string -> string
  (* [rpc_info interp] *)

(*s: ./commons/error.ml *)
open Common

(*s: class Error.t *)
(*e: class Error.t *)

class virtual t = object
 method virtual f : string -> unit
 method virtual ok : string -> unit
 method virtual choose : string -> bool
 method virtual ari : string -> int
end

class x = object
  inherit t
  method f _ = pr2 "TODO: Error.x.f"
  method ok _ = pr2 "TODO: Error.x.ok"
  method choose _ = failwith "TODO: Error.x.choose"
  method ari _ = failwith "TODO: Error.x.ari"
end

(*s: constant Error.default *)
let default = ref (new x)
(*e: constant Error.default *)

(* backward compatibility *)
(*s: functions Error.xxx *)
let f msg = 
  !default#f msg
let ok msg = 
  !default#ok msg
let choose msg = 
  !default#choose msg
let ari msg = 
  !default#ari msg
(*e: functions Error.xxx *)
(*e: ./commons/error.ml *)

(*s: ./commons/common.ml *)

(*s: constant Common.spf *)
let spf = Printf.sprintf
(*e: constant Common.spf *)

module StringSet = Set.Make(struct type t = string let compare = compare end)

(*s: function Common.TODOOPERATOR *)
let (|>) o f = f o
(*e: function Common.TODOOPERATOR *)
(*e: ./commons/common.ml *)

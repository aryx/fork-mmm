(*s: ./commons/common.ml *)

module StringSet = Set.Make(struct type t = string let compare = compare end)

(*s: constant Common.spf *)
let spf = Printf.sprintf
(*e: constant Common.spf *)

(*s: function Common.TODOOPERATOR *)
let (|>) o f = f o
(*e: function Common.TODOOPERATOR *)
(*e: ./commons/common.ml *)

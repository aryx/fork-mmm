
let spf = Printf.sprintf

module StringSet = Set.Make(struct type t = string let compare = compare end)

let (+>) o f = f o

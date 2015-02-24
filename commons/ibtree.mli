(*s: ./commons/ibtree.mli *)
module type S =
  sig
    type key
    type 'a t
    val empty: 'a t
    val add: (key * key) -> 'a -> 'a t -> 'a t
    val find: key -> 'a t -> 'a
    val find_interval : key -> 'a t -> key * key
  end

module Make(Ord: Map.OrderedType): (S with type key = Ord.t)

(*e: ./commons/ibtree.mli *)

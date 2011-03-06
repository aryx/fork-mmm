module Elements : Set.S with type elt = string

type t = {
  dtd_name : string;
  contents : (string, Elements.t) Hashtbl.t;
    (* for each element, give the set of included elements *)
  mutable open_omitted : Elements.t;
    (* set of elements for which opening tag may be omitted *)
  mutable close_omitted : Elements.t
    (* set of elements for which closing tag may be omitted *)
 } 

val dtd20 : t
val dtd32 : t
val dtd32f : t

(* A table of DTDs for preferences *)
val get : string -> t
val add : t -> unit
val name : t -> string

val names : unit -> string list

val current : t ref 

val dump : t -> unit

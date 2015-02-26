(*s: ./html/dtd.mli *)
module Elements : Set.S with type elt = string

(*s: type Dtd.t *)
type t = {
  dtd_name : string;
  contents : (string, Elements.t) Hashtbl.t;
    (* for each element, give the set of included elements *)

  mutable open_omitted : Elements.t;
    (* set of elements for which opening tag may be omitted *)
  mutable close_omitted : Elements.t
    (* set of elements for which closing tag may be omitted *)
 } 
(*e: type Dtd.t *)

(*s: signature Dtd.dtd20 *)
val dtd20 : t
(*e: signature Dtd.dtd20 *)
(*s: signature Dtd.dtd32 *)
val dtd32 : t
(*e: signature Dtd.dtd32 *)
(*s: signature Dtd.dtd32f *)
val dtd32f : t
(*e: signature Dtd.dtd32f *)

(*s: signature Dtd.get *)
(* A table of DTDs for preferences *)
val get : string -> t
(*e: signature Dtd.get *)
(*s: signature Dtd.add *)
val add : t -> unit
(*e: signature Dtd.add *)
(*s: signature Dtd.name *)
val name : t -> string
(*e: signature Dtd.name *)

(*s: signature Dtd.names *)
val names : unit -> string list
(*e: signature Dtd.names *)

(*s: signature Dtd.current *)
val current : t ref 
(*e: signature Dtd.current *)

(*s: signature Dtd.dump *)
val dump : t -> unit
(*e: signature Dtd.dump *)
(*e: ./html/dtd.mli *)

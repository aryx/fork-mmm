module Timer : sig
  type t

  val add : int -> (unit -> unit) -> t
  val set : int -> (unit -> unit) -> unit
  val remove : t -> unit
end

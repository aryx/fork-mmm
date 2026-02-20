(*s: www/maps.mli *)
(*s: type [[Maps.area_kind]] *)
(* The active areas *)
type area_kind = 
 | Rect 
 | Circle 
 | Poly 
 | Default
(*e: type [[Maps.area_kind]] *)

(*s: type [[Maps.area]] *)
(* The area *)
type area = {
  area_kind : area_kind;
  area_coords : int list;
  area_link : Hyper.link;
  area_alt  : string
 }
(*e: type [[Maps.area]] *)

(*s: type [[Maps.map]] *)
type map = area list
(*e: type [[Maps.map]] *)

(*s: type [[Maps.t]] *)
(* We merge any kind of map, for we actually are going to support
   maps for arbitrary embedded objects
 *)
type t = 
  | ClientSide of Hyper.link		(* usemap link *)
  | ServerSide of Hyper.link		(* ismap *)
  | Direct of Hyper.link			(* inside an anchor *)
  | NoMap				(* no additionnal navigation *)
  | FormMap of (int * int -> Hyper.link)
(*e: type [[Maps.t]] *)

(*s: type [[Maps.map_status]] *)
(* The table of client-side image maps *)
type map_status =
 | KnownMap of map
 | RequestedMap of string
(*e: type [[Maps.map_status]] *)

(*s: signature [[Maps.parse_coords]] *)
val parse_coords : string -> int list
(*e: signature [[Maps.parse_coords]] *)
(*s: signature [[Maps.get]] *)
val get : string -> map_status
(*e: signature [[Maps.get]] *)

val broadcast_backend: (string -> unit) ref
(*s: signature [[Maps.add]] *)
val add : string -> map -> unit
(*e: signature [[Maps.add]] *)
(*e: www/maps.mli *)

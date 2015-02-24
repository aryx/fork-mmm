(*s: ./www/maps.mli *)
(*s: enum Maps.area_kind *)
type area_kind = Rect | Circle | Poly | Default
(*e: enum Maps.area_kind *)

(*s: enum Maps.area *)
type area = {
  area_kind : area_kind;
  area_coords : int list;
  area_link : Hyper.link;
  area_alt  : string
 }
(*e: enum Maps.area *)

(*s: enum Maps.map *)
type map = area list
(*e: enum Maps.map *)

(*s: enum Maps.t *)
type t = 
    ClientSide of Hyper.link		(* usemap link *)
  | ServerSide of Hyper.link		(* ismap *)
  | Direct of Hyper.link			(* inside an anchor *)
  | NoMap				(* no additionnal navigation *)
  | FormMap of (int * int -> Hyper.link)
(*e: enum Maps.t *)

(*s: enum Maps.map_status *)
(* The table of client-side image maps *)
type map_status =
   KnownMap of map
 | RequestedMap of string
(*e: enum Maps.map_status *)

(*s: signature Maps.parse_coords *)
val parse_coords : string -> int list
(*e: signature Maps.parse_coords *)
(*s: signature Maps.get *)
val get : string -> map_status
(*e: signature Maps.get *)
(*s: signature Maps.add *)
val add : string -> map -> unit
(*e: signature Maps.add *)
(*e: ./www/maps.mli *)

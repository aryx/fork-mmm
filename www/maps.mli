type area_kind = Rect | Circle | Poly | Default

type area = {
  area_kind : area_kind;
  area_coords : int list;
  area_link : Hyper.link;
  area_alt  : string
 }

type map = area list

type t = 
    ClientSide of Hyper.link		(* usemap link *)
  | ServerSide of Hyper.link		(* ismap *)
  | Direct of Hyper.link			(* inside an anchor *)
  | NoMap				(* no additionnal navigation *)
  | FormMap of (int * int -> Hyper.link)

(* The table of client-side image maps *)
type map_status =
   KnownMap of map
 | RequestedMap of string

val parse_coords : string -> int list
val get : string -> map_status
val add : string -> map -> unit

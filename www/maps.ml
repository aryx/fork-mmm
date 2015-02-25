(*s: ./www/maps.ml *)
open Printf

(* Client-side image maps:
     the "only" difficulty in implementing client-side image maps is that
     the map may well come *after* the image in the document. In general,
     anyway, the map may be an arbitrary URL.

   We thus have to implement a general delay mechanism for maps : the idea
   here is to use a table of maps, each map being accessed by an URI (that is,
   an URL plus a fragment).

   PROBLEM: we have no idea in general when to flush this table.

 *)

(*s: type Maps.area_kind *)
(* The active areas *)
type area_kind = Rect | Circle | Poly | Default
(*e: type Maps.area_kind *)

(*s: type Maps.area *)
(* The area *)
type area = {
  area_kind : area_kind;
  area_coords : int list;
  area_link : Hyper.link;
  area_alt  : string
 }
(*e: type Maps.area *)

(*s: type Maps.map *)
type map = area list
(*e: type Maps.map *)

(*s: type Maps.t *)
(* We merge any kind of map, for we actually are going to support
   maps for arbitrary embedded objects
 *)
type t = 
    ClientSide of Hyper.link		(* usemap link *)
  | ServerSide of Hyper.link		(* ismap *)
  | Direct of Hyper.link			(* inside an anchor *)
  | NoMap				(* no additionnal navigation *)
  | FormMap of (int * int -> Hyper.link)
(*e: type Maps.t *)


(*s: type Maps.map_status *)
(* The table of client-side image maps *)
type map_status =
   KnownMap of map
 | RequestedMap of string
(*e: type Maps.map_status *)

(*s: constant Maps.table *)
let table = (Hashtbl.create 37 : (string, map_status) Hashtbl.t)
(*e: constant Maps.table *)

(*s: constant Maps.coord_sep *)
(* Tolerance: official syntax is "," separated.
   We use instead "[ \t\n]+\|\([ \t\n]*,[ \t\n]*\)"
   that is non empty sequence of whitespace
        or comma with possible surrounding whitespace
 *)
(* let coord_sep = Str.regexp "," *)
let coord_sep = Str.regexp "[ \t\n]+\|\([ \t\n]*,[ \t\n]*\)"
(*e: constant Maps.coord_sep *)
(*s: function Maps.parse_coords *)
let parse_coords s =
  List.map int_of_string (Str.split coord_sep s)
(*e: function Maps.parse_coords *)

(*s: function Maps.add *)
let add name map =
  Log.debug (sprintf "Adding map : %s" name);
  try
    match Hashtbl.find table name with
      KnownMap m -> Log.debug "Map already known !"
    | RequestedMap event ->
       Hashtbl.remove table name; (* remove it *)
       Hashtbl.add table name (KnownMap map); (* add its value *)
       Frx_synth.broadcast event (* trigger all waiting people *)
  with
    Not_found -> (* nobody requested it *)
      Hashtbl.add table name (KnownMap map)
(*e: function Maps.add *)

(*s: function Maps.get *)
let get name =
  Log.debug (sprintf "Asking map : %s" name);
  try
    Hashtbl.find table name 
  with
    Not_found ->
       let m = Mstring.gensym "map" in
         Hashtbl.add table name (RequestedMap m);
     RequestedMap m
(*e: function Maps.get *)
(*e: ./www/maps.ml *)

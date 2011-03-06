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

(* The active areas *)
type area_kind = Rect | Circle | Poly | Default

(* The area *)
type area = {
  area_kind : area_kind;
  area_coords : int list;
  area_link : Hyper.link;
  area_alt  : string
 }

type map = area list

(* We merge any kind of map, for we actually are going to support
   maps for arbitrary embedded objects
 *)
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

let table = (Hashtbl.create 37 : (string, map_status) Hashtbl.t)

(* Tolerance: official syntax is "," separated.
   We use instead "[ \t\n]+\|\([ \t\n]*,[ \t\n]*\)"
   that is non empty sequence of whitespace
        or comma with possible surrounding whitespace
 *)
(* let coord_sep = Str.regexp "," *)
let coord_sep = Str.regexp "[ \t\n]+\|\([ \t\n]*,[ \t\n]*\)"
let parse_coords s =
  List.map int_of_string (Str.split coord_sep s)

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

let get name =
  Log.debug (sprintf "Asking map : %s" name);
  try
    Hashtbl.find table name 
  with
    Not_found ->
       let m = Mstring.gensym "map" in
         Hashtbl.add table name (RequestedMap m);
	 RequestedMap m

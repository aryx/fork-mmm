(* Simple binary trees with no redondant elements, and no delete function *)

module type S =
  sig
    type key
    type 'a t
    val empty: 'a t
    val add: (key * key) -> 'a -> 'a t -> 'a t
    val find: key -> 'a t -> 'a
    val find_interval : key -> 'a t -> key * key
  end


module Make(Ord: Map.OrderedType) = struct

type key = Ord.t

type balance = Eq | Le | Ri

type 'a element = {
    interval : key * key;
    image : 'a
  } 

type 'a t =
 | Empty
 | Node of 'a node

and 'a node =
 {balance : balance; height : int;
  left : 'a t; element : 'a element; right : 'a t}

let height = function
  | Empty -> 0
  | Node {height = h} -> h

let create_node l e r =
  let hl = height l in
  let hr = height r in
  Node
   {
     balance = 
       (if hl = hr 
       then Eq 
       else 
         if hl < hr then Ri else Le
        )
     ;
     height = 1 + (if hr > hl then hr else hl);
     left = l; 
     element = e; 
     right = r;
   }

let turn_right = function
 | Empty -> Empty
 | Node
    {left =
      Node
       {balance = Eq | Ri;
        left = lle; element = le;
        right = Node {left = lrle; element = rle; right = rrle}};
     element = e; right = re} ->
      create_node (create_node lle le lrle) rle (create_node rrle e re)
 | Node
    {left =
      Node
       {balance = Le | Eq; left = lle; element = le; right = rle};
     element = e;
     right = re} -> create_node lle le (create_node rle e re)
 | _ -> failwith "turn_right"

and turn_left = function
 | Empty -> Empty
 | Node
    {left = le; element = e;
     right =
      Node
       {balance = Eq | Le;
        left = Node {left = llre; element = lre; right = rlre};
        element = re; right = rre}} ->
     create_node (create_node le e llre) lre (create_node rlre re rre)
 | Node
    {left = le; element = e;
     right =
      Node
       {balance = Ri | Eq; left = lre; element = re; right = rre}} ->
     create_node (create_node le e lre) re rre
 | _ -> failwith "turn_left"

let reball hr l e nr =
 let hnr = height nr and nt = create_node l e nr in
 if hnr > hr then turn_left nt else nt

let rebalr hl nl e r =
 let hnl = height nl and nt = create_node nl e r in
 if hnl > hl then turn_right nt else nt

(* Si les intervalles sont disjoints, la comparaison des bornes inferieures
   est un ordre *)

let compare_elements x y =
  Ord.compare (fst x.interval) (fst y.interval)

let rec insert x t =
 match t with
 | Empty ->
    Node {balance = Eq; height = 1; left = Empty; element = x; right = Empty}
 | Node {balance = b; left = l; element = e; right = r} ->
    let c = compare_elements x e in
    if c = 0 then t else
    if c > 0 then
     if b = Ri then reball (height r) l e (insert x r)
      else create_node l e (insert x r) else
    if b = Le then rebalr (height l) (insert x l) e r
     else create_node (insert x l) e r


let empty = Empty

let rec find pos = function
| Empty -> raise Not_found
| Node {left = l; element = e ; right = r} ->
   let c_start = Ord.compare pos (fst e.interval) in
   if c_start < 0 then find pos l else
   let c_stop = Ord.compare pos (snd e.interval) in
   if c_stop < 0 then e.image else find pos r


let rec find_interval pos = function
| Empty -> raise Not_found
| Node {left = l; element = e ; right = r} ->
   let c_start = Ord.compare pos (fst e.interval) in
   if c_start < 0 then find_interval pos l else
   let c_stop = Ord.compare pos (snd e.interval) in
   if c_stop < 0 then e.interval else find_interval pos r


let add i v = insert {interval = i; image = v}


(* Application to anchors
type position = int * int

type anchor_position = {start : position; stop : position}

let rec anchor_of_pos pos = function
| Empty -> raise Not_found
| Node {left = l; element = e; right = r} ->
   let c_start = compare pos e.start in
   if c_start < 0 then anchor_of_pos pos l else
   let c_stop = compare pos e.stop in
   if c_stop < 0 then e else anchor_of_pos pos r

let anchors_table = ref Empty

let add_anchor a = anchors_table := insert a !anchors_table
let find_anchor mouse_pos = anchor_of_pos mouse_pos !anchors_table

 *)
(* Exemples
let anchors = [
 {start = 1,3; stop = 1,7};
 {start = 1,13; stop = 1,17};
 {start = 1,30; stop = 1,70};
 {start = 10,3; stop = 10,7};
 {start = 11,3; stop = 12,1};
 {start = 12,3; stop = 13,17};
 {start = 14,30; stop = 15,7};
 {start = 100,3; stop = 100,7};
 {start = 101,3; stop = 101,7};
 {start = 101,31; stop = 101,37}
 ]
do_list add_anchor anchors

find_anchor (1,6)
find_anchor (12,0)
find_anchor (11,7)
find_anchor (101,3)
find_anchor (15,3)
find_anchor (16,3)

(* A bit long (more than 3 seconds in Caml Light) *)
for i = 0 to 10000 do 
 add_anchor {start = i, 1+i/2; stop = i, i}
done
*)
end

(*
 * List utilities
 *)
(* tln l n *)
let rec tln l = function
   0 -> l
 | n -> if l = [] then [] else tln (List.tl l) (pred n)

let hdn l =
  let rec h l acc = function
    0 -> List.rev acc
  | n -> if l = [] then List.rev acc 
      	 else h (List.tl l) (List.hd l :: acc) (pred n) in
  h l []

let except_assoc x =
  let rec ex acc = function 
      [] -> acc
    | (y,v)::l when x = y -> ex acc l
    | z :: l -> ex (z::acc) l
  in
  ex []

let exceptq x =
  let rec ex acc = function
     [] -> acc
   | y::l when y == x -> ex acc l
   | y::l -> ex (y::acc) l
  in
  ex []



(* List.iter from right to left *)
let rev_do_list f = 
 let rec do_list_f = function
     [] -> () | x::l -> do_list_f l; f x in
  do_list_f


let rec do_listi f n l =
  match l with
    [] -> ()
  | (x::l) -> f n x; do_listi f (succ n) l


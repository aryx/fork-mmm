(*s: commons/mlist.ml *)
(*
 * List utilities
 *)
(*s: function [[Mlist.tln]] *)
(* tln l n *)
let rec tln l = function
   0 -> l
 | n -> if l = [] then [] else tln (List.tl l) (pred n)
(*e: function [[Mlist.tln]] *)

(*s: function [[Mlist.hdn]] *)
let hdn l =
  let rec h l acc = function
    0 -> List.rev acc
  | n -> if l = [] then List.rev acc 
        else h (List.tl l) (List.hd l :: acc) (pred n) in
  h l []
(*e: function [[Mlist.hdn]] *)

(*s: function [[Mlist.except_assoc]] *)
let except_assoc x =
  let rec ex acc = function 
      [] -> acc
    | (y,_v)::l when x = y -> ex acc l
    | z :: l -> ex (z::acc) l
  in
  ex []
(*e: function [[Mlist.except_assoc]] *)

(*s: function [[Mlist.exceptq]] *)
let exceptq x =
  let rec ex acc = function
     [] -> acc
   | y::l when y == x -> ex acc l
   | y::l -> ex (y::acc) l
  in
  ex []
(*e: function [[Mlist.exceptq]] *)



(*s: function [[Mlist.rev_do_list]] *)
(* List.iter from right to left *)
let rev_do_list f = 
 let rec do_list_f = function
     [] -> () | x::l -> do_list_f l; f x in
  do_list_f
(*e: function [[Mlist.rev_do_list]] *)


(*s: function [[Mlist.do_listi]] *)
let rec do_listi f n l =
  match l with
    [] -> ()
  | (x::l) -> f n x; do_listi f (succ n) l
(*e: function [[Mlist.do_listi]] *)

(*e: commons/mlist.ml *)

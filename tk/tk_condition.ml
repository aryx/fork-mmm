(* Conditions *)

(* We don't take any chance with the semantics of tkwait variable, in the
   sense that we make sure that the value changes each time we set the
   condition
 *)
open Printf
open Condition

type t = Textvariable.textVariable * int ref

let h = Hashtbl.create 101

let backend () = { Condition.
 create = (fun s ->
    let x = Textvariable.create(), ref 0 in
    Hashtbl.add h s x
  );
 set = (fun s ->
   let (v , r) = Hashtbl.find h s in
   incr r; Textvariable.set v (sprintf "cond%d" !r);
 );
   
 wait = (fun s ->
   let (v , _) = Hashtbl.find h s in
   Tkwait.variable v
 );

 free = (fun s ->
   let (v, _) = Hashtbl.find h s in
   Textvariable.free v
 );
 
}


  

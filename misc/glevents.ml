open Printf
open Tk

(* A global table for describing events
 * TODO: use virtual events because here we don't change bindings in 
 * place after a preference reload
 *)

let events = Hashtbl.create 37

let builtin_defaults = [
  (* tachymeter bindings *)
  "tachy_about", [[], ButtonPressDetail 3];
  "tachy_gc",[[], KeyPressDetail "g"; [], KeyPressDetail "c"];
  "tachy_new", [[], ButtonPressDetail 1];
  "tachy_sel", [[], ButtonPressDetail 2];
  (* bindings on inlined images *)
  "loadimage", [[Control], ButtonPressDetail 1];
  "alt_imap", [[],ButtonPressDetail 1];	(* alt mode client side img map *)
  "stopanim",  [[], ButtonPressDetail 2];
  "restartanim", [[Shift], ButtonPressDetail 2];
  "copyimgurl", [[], ButtonPressDetail 2];
  "updateimage", [[Shift], ButtonPressDetail 2];
  (* anchor bindings *)
  "goto", [[], ButtonPressDetail 1];
  "save", [[Shift], ButtonPressDetail 1];
  "gotonew", [[], ButtonPressDetail 3];
  "hypermenu", [[Control], ButtonPressDetail 1];
]

let get = Hashtbl.find events

(* This is for preferences *)
let reset () =
  Hashtbl.clear events;
  (* Now: for all names defined in defaults, check a possible overriding value
     in resources *)
  List.iter (fun (name,default) ->
    Hashtbl.add events 
      name (Tkresource.event_sequence (sprintf "bind<%s>" name) default))
    builtin_defaults
    
  

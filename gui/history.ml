(* History *)
open Document

(* 
   Linear history: we keep going adding to the end of the list,
   EXCEPT when you go back and then on a new link.
*)


type history_entry = {
  h_did : document_id;
  h_fragment : string option;
  h_prev : history_entry option;
  mutable h_next : history_entry option
  }

type t = {
  h_key : int;
  mutable h_start : history_entry;
  mutable h_current: history_entry;
  mutable h_first : bool
  }

let contents h =
  let l = ref [] in
  let rec walk e =
    l := e :: !l;
    match e.h_next with
      	None -> !l
      | Some e -> walk e
  in walk h.h_start

(* Did made obsolete by history overwriting *)
(* Since a did may occur several times in the history, the list of
   obsolete entries is not simply the overwritten entries *)

let obsolete current next =
  let kept = ref DocumentIDSet.empty
  and forgotten = ref DocumentIDSet.empty in
  let rec back e =
    kept := DocumentIDSet.add e.h_did !kept;
    match e.h_prev with
      None -> ()
    | Some e -> back e in
  let rec forw e =
    forgotten := DocumentIDSet.add e.h_did !forgotten;
    match e.h_next with
      None -> ()
    | Some e -> forw e in
  back current;
  forw next;
  DocumentIDSet.diff !forgotten !kept

(* Add hinfo to the current point *)
let add h did frag =
  (* Hack for the initial document *)
  if h.h_first then begin
    let newe = {h_did = did;
		h_fragment = frag;
		h_prev = None;
		h_next = None} in
      h.h_start <- newe;
      h.h_current <- newe;
      h.h_first <- false
      end
  else
    match h.h_current.h_next with
      None -> (* last in the list *)
	(* the new entry *)
	let newe = {h_did = did;
		    h_fragment = frag;
		    h_prev = Some h.h_current;
		    h_next = None} in
	(* fix the linked list *)
	h.h_current.h_next <- Some newe;
	(* set the new current *)
	h.h_current <- newe
   | Some e -> (* adding in the middle of the list *)
	let newe = {h_did = did;
		    h_fragment = frag;
		    h_prev = Some h.h_current;
		    h_next = None} in
	let dropped = obsolete newe e in
	h.h_current.h_next <- Some newe;
	h.h_current <- newe;
	DocumentIDSet.iter (Gcache.remove h.h_key) dropped

let create =
  let keycnter = ref 0 in
  (fun did ->
    let e = { h_did = did;
      	      h_fragment = None;
      	      h_prev = None;
	      h_next = None} in
    { h_key = (incr keycnter; !keycnter);
      h_start = e;
      h_current = e;
      h_first = true
    })

let back h =
    match h.h_current.h_prev with
      	None -> None
      | Some e -> h.h_current <- e; Some (e.h_did, e.h_fragment)

let forward h =
    match h.h_current.h_next with
      	None -> None
      | Some e -> h.h_current <-e ; Some (e.h_did, e.h_fragment)

let set_current h e =
  h.h_current <- e


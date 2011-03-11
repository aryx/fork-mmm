(* easy balloon help facility *)
open Tk
open Widget
open Protocol

(* switch -- if you do not want balloons, set false *)
let flag = ref true
let debug = ref false

(* We assume we have at most one popup label at a time *)
let topw = ref default_toplevel
and popupw = ref default_toplevel

let configure_cursor w cursor = 
  (* DDDDDDDDDIIIIIIIRRRRRRRRTTTTTTTTYYYYYYY *)
  Protocol.tkCommand
    [| TkToken (name w); 
       TkToken "configure";
       TkToken "-cursor";
       TkToken cursor |]

let put w millisec mesg = 
  let t = ref None in
  let cursor = ref "" in

  let reset () = 
      begin
  	match !t with
  	  Some tim -> 
	    Timer.remove tim; 
	    t := None
  	| _ -> ()
      end;
      (* if there is a popup label, unmap it *)
      if Winfo.exists !topw && Wm.state !topw <> "withdrawn" then 
	begin
	  Wm.withdraw !topw;
	  if Winfo.exists w then configure_cursor w !cursor
	end
  and set ev =
    if !flag then
      t := Some (Timer.add millisec (fun () -> 
	t := None;
	if !debug then
	  prerr_endline ("Balloon: " ^ Widget.name w);
	update_idletasks ();
	Message.configure !popupw [Text mesg]; 
	raise_window !topw;
	Wm.geometry_set !topw (* 9 & 8 are some kind of magic... *)
	  ("+" ^ string_of_int (ev.ev_RootX + 9) ^
	   "+" ^ string_of_int (ev.ev_RootY + 8));
	Wm.deiconify !topw;
	cursor := cget w CCursor;
	configure_cursor w "hand2"))
  in

  List.iter (fun x ->
    bind w x (BindExtend ([], (fun _ -> 
(*      begin
	match x with
	  [[],Leave] -> prerr_endline " LEAVE reset "
	| _ -> prerr_endline " Other reset "
      end; 
*)
      reset ()))))
      [[[], Leave]; [[], ButtonPress]; [[], ButtonRelease]; [[], Destroy];
       [[], KeyPress]; [[], KeyRelease]];
  List.iter (fun x ->
    bind w x (BindExtend ([Ev_RootX; Ev_RootY], (fun ev -> 
(*
      begin
	match x with
	  [[],Enter] -> prerr_endline " Enter set "
	| [[],Motion] -> prerr_endline " Motion set "
	| _ -> prerr_endline " ??? set "
      end;
*)
      reset (); set ev))))
      [[[], Enter]; [[], Motion]]

let init () =
  let t = Hashtbl.create 101 in
  Protocol.add_destroy_hook (fun w ->
    Hashtbl.remove t w);
  topw := Toplevel.create default_toplevel [];
  Wm.overrideredirect_set !topw true;
  Wm.withdraw !topw;
  class_bind "all" [[], Enter] (BindExtend ([Ev_Widget], (function w ->
    try Hashtbl.find t w.ev_Widget with
      Not_found -> begin
	(* we have to create the message widget here ! *)
	if !popupw == default_toplevel then begin
	  popupw := Message.create_named !topw "balloon" 
	       [Background (NamedColor "yellow"); Aspect 300];
	  pack [!popupw] []
	end;
	Hashtbl.add t w.ev_Widget ();
	let x = Resource.get w.ev_Widget "balloon" "Balloon" in
	if x <> "" then put w.ev_Widget 1000 x
      end)))


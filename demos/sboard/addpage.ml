open Unix
open Tk
open Tree

(* External requests *)
(* This tool *can* be used with other softwares than mmm, so it should have
   its own request port *)
 
let init_external tree =
 let file = Misc.user_file ".mmm/surfport" in
 try
  let socket = socket PF_UNIX SOCK_STREAM 0 in
    begin try Unix.bind socket (ADDR_UNIX file) with
    | _ -> 
	Unix.access file [F_OK]; 
	begin match 
	  Frx_dialog.f Widget.default_toplevel 
	    (Mstring.gensym "confirm")
	    "Confirm"
	    (Printf.sprintf "%s already exists. This may mean that there is another Surfboard already running. Do you want to remove this file and create again ? (Note that you must be sure there is no other Surfboard)" file)
	    (Predefined "question") 0
	    [ "Yes"; "No, I don't mind." ] 
	with
	  0 -> 
	    Unix.unlink file;  
	    Unix.bind socket (ADDR_UNIX file) 
	| _ -> raise Exit
	end
    end;
    listen socket 5;
    Fileevent.add_fileinput socket
      	(fun () -> 
	  try 
      	   let fd,_ = accept socket in
	   let request = Munix.read_line fd in
	   close fd; 
	   (* request must be *)
	   (* URL<space>TITLE *) 
	   let url, title =
	     let pos = String.index request ' ' in
	     String.sub request 0 (pos - 1),
	     String.sub request (pos + 1) (String.length request - (pos + 1))
	   in
	   let page = new page () in
	   page#set_url url;
	   page#set_title title;
	   (* page#set_adddate / page#set_lastvisit *)
	   tree#add_page page;
	   (* gui refresh *)
	   Treeview.insert_for_all_viewers (page :> item)
	  with _ -> ());
    at_exit (fun () -> Msys.rm file)
 with
   Exit -> ()
 | _ ->
     !Error.default#f (Printf.sprintf "Can't initialize %s" file)

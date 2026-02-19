(*s: gui/cci.ml *)
open Fpath_.Operators
open I18n
open Tk
open Unix

(* pad: CCI = Common Client Interface? *)

(*s: function [[Cci.handler]] *)
(* CCI was cool, but nobody implements it anymore. More over,
 * it's trivial to fork mmm_remote and let the protocol be managed
 * by it *)

let handler (caps: < Cap.network; ..>) (fd : Unix.file_descr) (line : string) =
  let len = String.length line in 
  if len > 4 && String.sub line 0 4 = "GET " then begin
    let url = String.sub line 4 (len - 4) in
    match !Mmm.main_navigator with
      None -> 
    Munix.write_string fd "No main navigator\n";
    close fd
    | Some nav ->
    Nav.save_link nav (Some (fd, true))
      {h_uri = url; h_context = None; h_method = GET; h_params = []}
  end else if len > 5 && String.sub line 0 5 = "GETB " then begin
    let url = String.sub line 5 (len - 5) in
    match !Mmm.main_navigator with
      None ->
    Munix.write_string fd "No main navigator\n";
    close fd
    | Some nav ->
    Nav.save_link nav (Some (fd, false))
      {h_uri = url; h_context = None; h_method = GET; h_params = []}
  end else if len > 5 && String.sub line 0 5 = "HEAD " then begin
    let url = String.sub line 5 (len - 5) in
    match !Mmm.main_navigator with
      None ->
    Munix.write_string fd "No main navigator\n";
    close fd
    | Some nav ->
    Nav.save_link nav (Some (fd, true))
      {h_uri = url; h_context = None; h_method = HEAD; h_params = []}
  end else if len > 8 && String.sub line 0 8 = "DISPLAY " then begin
    let url = String.sub line 8 (len - 8) in
    close fd;
    ignore (Mmm.navigator caps false (Lexurl.make url))
  end else begin (* assume DISPLAY (backward compatibility) *)
    close fd;
    ignore (Mmm.navigator caps false (Lexurl.make line))
  end
(*e: function [[Cci.handler]] *)

(*s: function [[Cci.init]] *)
(* External requests *)
let init (caps : < Cap.network; .. >) =
 let file = Mmm.user_file "remote" in
 try
  let socket = socket PF_UNIX SOCK_STREAM 0 in
    begin try bind socket (ADDR_UNIX !!file) 
    with
      _ -> 
    if not (Sys.file_exists !!file) then raise Not_found;
    begin match 
      Frx_dialog.f Widget.default_toplevel 
        (Mstring.gensym "confirm")
        (s_ "Confirm")
        (s_ "%s already exists. This may mean that there is another MMM already running. Do you want to remove this file and create again ? (Note that you must be sure there is no other MMM with -external option)" !!file)
        (Predefined "question") 0
        [ s_ "Yes"; 
          s_ "No, I give up to use -external option"] 
    with
      0 -> 
        Unix.unlink !!file;  
        bind socket (ADDR_UNIX !!file) 
    | _ -> raise Exit
    end
    end;
    
    listen socket 5;
    Fileevent.add_fileinput socket
       (fun () -> 
      try 
          let fd,_ = accept socket in
          handler caps fd (Munix.read_line fd)
      with _ -> ());
    at_exit (fun () -> Msys.rm !!file)
 with e ->
   Error.f (s_ "Can't initialize %s\n%s" !!file (Printexc.to_string e))
(*e: function [[Cci.init]] *)

(*e: gui/cci.ml *)

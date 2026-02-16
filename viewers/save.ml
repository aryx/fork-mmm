(*s: ./viewers/save.ml *)
open I18n
open Unix
open Document
open Url
open Www
open Feed

(* Save to file fname, and apply continuation cont to this file *)
(*s: function Save.f *)
(* unprotected against Sys_error *)
let f cont dh fname endmsg =
  let oc = open_out_bin fname in
  let buffer = Bytes.create 1024 in
  let red = ref 0 in
  let size =   
    try Http_headers.contentlength dh.dh_headers
    with Not_found -> 40000 (* duh *)
  in
  dh.document_feed.feed_schedule
    (fun () ->
      try
    let n = dh.document_feed.feed_read buffer 0 1024 in
    if n = 0 then begin
      dclose true dh;
      close_out oc;
      Document.end_log dh endmsg;
      cont fname (* cont is responsible for deleting fname *)
    end
    else begin
          output oc buffer 0 n;
         red := !red + n;
      Document.progress_log dh (!red * 100 / size)
    end
      with
    Unix_error(_,_,_) | Sys_error _ ->
      dclose true dh;
      close_out oc;
      Document.destroy_log dh false;
      Msys.rm fname;
      Error.f (s_ "Error during retrieval of %s" 
                     (Url.string_of dh.document_id.document_url))
     )
(*e: function Save.f *)

(*s: function Save.tofile *)
(* Used for external viewers in batch mode. Deprecated *)
let tofile cont dh fname endmsg =
  try
    f cont dh fname endmsg
  with Sys_error msg -> 
    dclose true dh;
    Document.destroy_log dh false;
    Error.f (s_ "Cannot save to %s\n(%s)" fname msg)
(*e: function Save.tofile *)

(*s: function Save.interactive *)
let rec interactive cont dh =
  (* The initial content of the requester *)
  let url = Url.string_of dh.document_id.document_url in
  let path = 
    match dh.document_id.document_url.path with Some p -> p | None -> "" in

  Fileselect.f (s_ "Save document") (function 
    | [] ->
         (* by closing dh, we might break the cache *)
         dclose true dh
    | [fname] ->
        begin try 
          let endmsg = (s_ "URL %s\nsaved as %s" url fname) in
          f cont dh fname endmsg;
          Document.add_log dh 
              (s_ "Saving %s\nto %s" url fname)
              (* channel is not closed ! *)
              (fun () -> Msys.rm fname)
        with Sys_error msg -> 
          Error.f (s_ "Cannot save to %s\n(%s)" fname msg);
          interactive cont dh
        end
    | _l -> raise (Failure "multiple selection")
    )
    "*"
    (Filename.basename path)
    false false
(*e: function Save.interactive *)


(*s: function Save.transfer *)
let transfer wr dh dest =
  wr.www_logging (s_ "Saving...");
  match dest with
    None -> interactive (fun _s -> wr.www_logging "") dh
  | Some (fd, flag) ->
      (* if flag we should output the headers as well *)
      if flag then begin
    dh.dh_headers |> List.rev |> List.iter (fun h -> 
      Munix.write_string fd h; Munix.write_string fd "\n"
    );
    Munix.write_string fd "\n";
      end;
      let buffer = Bytes.create 1024 in
      dh.document_feed.feed_schedule
       (fun () ->
      try
        let n = dh.document_feed.feed_read buffer 0 1024 in
        if n = 0 then begin
          dclose true dh;
          close fd;
        end
     else ignore (write fd buffer 0 n)
    with
     Unix_error(_,_,_) | Sys_error _ ->
       dclose true dh;
       close fd;
       Error.f (s_"Error during retrieval of %s" 
                    (Url.string_of dh.document_id.document_url))
       )
(*e: function Save.transfer *)

(*s: function Save.save_from_string *)
let save_from_string url s f =
  try
    let oc = open_out_bin f in
    begin try
      output_string oc s;
      Error.ok (s_ "Document %s\nsaved in\n%s" (Url.string_of url) f)
    with Sys_error e ->
      Error.f (s_ "Cannot save to %s\n(%s)" f e)
   end;
   close_out oc
  with Sys_error e ->
    Error.f (s_ "Cannot save to %s\n(%s)" f e)
(*e: function Save.save_from_string *)

(*s: function Save.copy_file *)
let copy_file url src dst =
  try
    let ic = open_in_bin src
    and oc = open_out_bin dst 
    and buf = Bytes.create 2048 in
    let rec copy () =
      let n = input ic buf 0 2048 in
      if n <> 0 then begin output oc buf 0 n; copy() end
    in
    begin try 
      copy();
      Error.ok (s_ "Document %s\nsaved in\n%s" (Url.string_of url) dst)
    with Sys_error e ->
      Error.f (s_ "Cannot save to %s\n(%s)" dst e)
    end;
    close_in ic; 
    close_out oc
  with Sys_error e ->
    Error.f (s_ "Cannot save to %s\n(%s)" dst e)
(*e: function Save.copy_file *)


(*s: function Save.pipe_from_string *)
(* Cmd can be composite. We add the URL at the end *)
let pipe_from_string url data cmd =
  let urls = Url.string_of url in
  try
    (* we have to open a pipe and write to it *)
    let fd_in, fd_out = pipe() in
    let len = String.length data and pos = ref 0 in
    (* now fork the command *)
    match Low.fork() with
      0 ->
    dup2 fd_in stdin; close fd_in; close fd_out;
    ignore (Munix.system_eval cmd ["URL", urls] false);
    exit 0
    | _n ->
    close fd_in;
       Fileevent.add_fileoutput fd_out (fun () ->
      if !pos < len then begin
        let n = min 512 (len - !pos) in
        try
          let w = write fd_out (Bytes.of_string data) !pos n in
          pos := !pos + w
        with
          Unix_error (_,_,_) -> (* can't write *)
        Fileevent.remove_fileoutput fd_out;
        close fd_out;
        Error.f (s_ "Error during |%s in %s" cmd urls)
      end else begin (* we're done *)
        Fileevent.remove_fileoutput fd_out;
        close fd_out
      end)
  with
  | Unix_error(_,_,_) -> (* pipe failed, fork failed *)
      Error.f (s_ "Can't execute command %s for %s" cmd urls)
(*e: function Save.pipe_from_string *)


(*s: function Save.pipe_from_file *)
let pipe_from_file url f cmd =
  let urls = Url.string_of url in
  try
    (* just open the file and read from it *)
    match Low.fork() with
      0 ->
    let fd = openfile f [O_RDONLY] 0 in
    dup2 fd stdin; close fd;
    ignore (Munix.system_eval cmd ["URL", urls] false);
    exit 0
    | _n ->
    ()
  with Unix_error(_,_,_) -> (* pipe failed, fork failed *)
    Error.f (s_ "Can't execute command %s for %s" cmd urls)
(*e: function Save.pipe_from_file *)

(*s: function Save.document *)
let document did arg =
  let open_selection_box act =
    Fileselect.f (s_ "Save or pipe to file")
      (function [] -> ()
             | [s] -> act s
          | _l -> raise (Failure "multiple selection"))
      "*" (* should be better *)
      (Filename.basename (Url.string_of did.document_url))
      false
      true
  in
  let proceed f = match arg with
    None -> open_selection_box f
  | Some s -> f s
  in
  try
    match Cache.find did with
      {document_data = MemoryData buf; _} ->
        proceed
      (fun s ->
        if String.length s <> 0 && s.[0] == '|' then
          pipe_from_string did.document_url (Ebuffer.get buf)
        (String.sub s 1 (String.length s - 1))
        else
          save_from_string did.document_url (Ebuffer.get buf) s)
      
    |  {document_data = FileData (f, _); _} ->
        proceed 
      (fun s ->
        if String.length s <> 0 && s.[0] == '|' then
          pipe_from_file did.document_url f 
        (String.sub s 1 (String.length s - 1))
        else
          copy_file did.document_url f s)
  with Not_found ->
    Error.f ("Document is not in cache.")
(*e: function Save.document *)
    
(*s: constant Save.print_command *)
let print_command = ref ""
(*e: constant Save.print_command *)
(*e: ./viewers/save.ml *)

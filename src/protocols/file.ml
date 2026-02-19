(*s: protocols/file.ml *)
(* The file: protocol *)
open I18n
open Printf
open Unix
open Filename
open Hyper
open Www
open Url
open Messages
open Http_headers
open Http
open Document
open Feed

(*s: exception [[File.File_error]] *)
exception File_error of string
(*e: exception [[File.File_error]] *)

(*s: function [[File.isdir]] *)
(* 
 * Simulate directory
 *)
let _isdir path f =
  let fullname = Filename.concat path f in
  (stat fullname).st_kind = S_DIR
(*e: function [[File.isdir]] *)

(*s: function [[File.d2html]] *)
let d2html path (d : Unix.dir_handle) =
  (* make sure that when path is used in url, it is / terminated *)
  let pathurl =
    let l = String.length path in
    if l = 0 
    then path 
    else
      if path.[l-1] = '/' 
      then path
      else sprintf "%s/" path
  in
  Printf.printf 
"<HTML>
<HEAD><TITLE>%s</TITLE>
<BASE HREF=\"file://localhost%s\">
</HEAD>
<BODY>
<H1>Directory list: %s</H1>
<PRE>" path pathurl path;

  let entries = ref [] in
  begin 
   try
     while true do 
       entries := (readdir d) :: !entries
     done      	
   with  End_of_file -> closedir d
  end;
  entries := List.sort Stdlib.compare !entries;
  !entries |> List.iter (function
    |  "." -> ()
    | ".." ->
       printf "Dir   <A HREF=\"file://localhost%s\">..</A>\n"
              (Filename.concat (dirname (dirname pathurl)) "")
    | f ->
       try
         let fullname = Filename.concat path f in
         let st = stat fullname in
         match st.st_kind with
         | S_DIR -> printf "Dir   <A HREF=\"%s\">%s</A>\n" f f
         | S_REG -> printf "File  <A HREF=\"%s\">%-30s</A>%8d bytes\n" 
                       f f (st.st_size)
         | S_LNK -> printf "Link  <A HREF=\"%s\">%s</A>\n" f f
         | _ -> ()
       with Unix_error(_,_,_) -> ()
    );
  printf "</PRE></BODY></HTML>"
(*e: function [[File.d2html]] *)

(*s: function [[File.dir]] *)
(* It's easiest to do it asynchronously anyway *)
let dir path =
  try
    let d = opendir path in
    let cin, cout = pipe() in
    match Low.fork() with
    (* child *)
    | 0 -> 
        close cin; dup2 cout stdout; close cout;
        begin
          try 
            d2html path d 
          with e ->
            print_endline (Printexc.to_string e)
        end;
        flush Stdlib.stdout; (* strange bug with our at_exit stuff *)
        (* nosemgrep: do-not-use-exit *)
        exit 0
        (*cin (*duh*) *)
     (* parent *)
     | _n -> closedir d; close cout; cin
  with Unix_error(_,_,_)  -> 
    raise (File_error (s_ "cannot open dir"))
(*e: function [[File.dir]] *)
  

(*s: function [[File.document_id]] *)
let document_id wwwr =
  { document_url = wwwr.www_url; document_stamp = no_stamp}
(*e: function [[File.document_id]] *)

(*s: function [[File.fake_cgi]] *)
(* Not true CGI interface, just a hack *)
(* TODO: headers ? *)
let fake_cgi wwwr cont path =
  try 
    let (cmd_in, cmd_out) = pipe() in
    let cmd, args = 
      try 
       let pos = String.index path '?' in
       let cmd = String.sub path 0 pos in
       if pos + 1 = String.length path 
       then cmd, [| cmd |]
       else cmd, [|cmd; String.sub path (pos+1) (String.length path - pos - 1)|]
      with Not_found -> path, [| path |] 
    in
    match Low.fork() with
    (* child *)
    | 0 -> 
        close cmd_in;
        dup2 cmd_out stdout; close cmd_out;
        begin 
          try execvp cmd args 
          with Unix_error(e, _, _) ->
            Munix.write_string stdout "HTTP/1.0 404 Not found\r\n";
            Munix.write_string stdout "Content-Type: text/html\r\n\r\n";
            Munix.write_string stdout "<H1>Cannot execute local file</H1>";
            Munix.write_string stdout "Command \"";
            Munix.write_string stdout cmd;
            Munix.write_string stdout "\" failed:";
            Munix.write_string stdout (Unix.error_message e);
            Munix.write_string stdout "\n";
            (* nosemgrep: do-not-use-exit *)
            exit 1
         end
   (* parent *)
   | _n ->
      close cmd_out;
      let dh = {document_id = document_id wwwr;
                document_referer = wwwr.www_link.h_context;
                document_status = 0;
                dh_headers = [];
                document_feed = Feed.of_fd cmd_in;
                document_fragment = wwwr.www_fragment;
                document_logger = Document.tty_logger} 
      in
      dh.document_feed.feed_schedule
        (fun () ->
           try
            if dh.dh_headers = [] then begin
            (* it should be the HTTP Status-Line *)
            let l = Low.read_line cmd_in in
              dh.document_status <- (parse_status l).status_code;
              dh.dh_headers <- [l] (* keep it there *)
            end
          else 
            dh.dh_headers <- 
              read_headers cmd_in dh.dh_headers
          with
           | End_of_headers ->
               dh.document_feed.feed_unschedule();
               cont.document_process dh
           | Not_found -> (* No HTTP/ header *)
               dh.document_feed.feed_unschedule();
               dh.document_status <- 200;
               dh.dh_headers <- ["Content-Type: text/plain"];
               cont.document_process dh
           | Unix_error(_,_,_) ->
               dclose true dh;
               raise (File_error (s_ 
                  "Error while reading headers of %s\n%s" path "(read)"))
           | Invalid_HTTP_header s ->
               dclose true dh;
               raise (File_error (s_ 
                      "Error while reading headers of %s\n%s" path s))
           | End_of_file ->
               dclose true dh;
               raise (File_error (s_ 
                  "Error while reading headers of %s\n%s" path "eof"))
          )
  with Unix_error(_,_,_) -> 
    raise (File_error (s_ "cannot exec file"))
(*e: function [[File.fake_cgi]] *)

(*s: constant [[File.binary_path]] *)
(* Pref stuff *)
let binary_path = ref ([] : string list)
(*e: constant [[File.binary_path]] *)
(*s: constant [[File.r]] *)
let _r = Str.regexp ":"
(*e: constant [[File.r]] *)
(*s: function [[File.pref_init]] *)
(*e: function [[File.pref_init]] *)
(*s: function [[File.pref_set]] *)
(*e: function [[File.pref_set]] *)

(*s: function [[File.is_cgi]] *)
let is_cgi file =
  match !binary_path with
  | [] -> false
  | path ->
      let l = String.length file in
      List.exists (fun dir ->
    let ldir = String.length dir in
    l > ldir && String.sub file 0 ldir = dir)
    path
(*
 * Display a file on the local unix file system (file:)
 *  is path really supposed to be absolute ?
 * Note: completely ignores method (GET, POST,...)
 *)
(*e: function [[File.is_cgi]] *)
(*s: function [[File.request]] *)
(*
 * Display a file on the local unix file system (file:)
 *  is path really supposed to be absolute ?
 * Note: completely ignores method (GET, POST,...)
 *)

let request wr cont =
  let path = 
    match wr.www_url.path with
    | Some path -> "/" ^ (Lexurl.remove_dots path)
    | None -> "/" 
  in
  (*s: [[File.request()]] if CGI path *)
  if is_cgi path 
  then (fake_cgi wr cont path; (fun () -> ()))
  (*e: [[File.request()]] if CGI path *)
  else   (* A bit weird, but we don't want to capture errors from the cont *)
    let st =
      try stat path 
      with _ -> raise (File_error (s_ "cannot stat file")) 
    in
    match st.st_kind with
    | S_REG ->
      begin
      (* check if this is an update *)
        try 
          let since = get_header "if-modified-since" wr.www_headers in
          let ht = Lexdate.ht_of_string since in
          let filet = Http_date.ht_of_stamp st.st_mtime in
          if Http_date.compare filet ht > 0
          then raise Not_found (* fall through *)
          else begin
            let dh = { 
              document_id = document_id wr;
              document_referer = wr.www_link.h_context;
              document_status = 304;
              dh_headers = [ sprintf "Date: %s" (Date.asc_now())];
              document_feed = 
                Feed.of_fd (openfile "/dev/null" [O_RDONLY] 0);
              document_fragment = wr.www_fragment;
              document_logger = Document.tty_logger
            } in

            Retype.f dh; (* pad: to get the content type based on the suffix *)

            cont.document_process dh;
            (fun () -> ())
          end
       with
       | Not_found  (* default case *)
       | Lexdate.Invalid_date (_,_) ->
          let s = 
            try openfile path [O_RDONLY] 0
            with Unix_error(_,_,_) -> 
              raise (File_error (s_ "cannot open file")) 
          in
          let dh =
            { document_id = document_id wr;
              document_referer = wr.www_link.h_context;
              document_status = 200;
              dh_headers = 
                [sprintf "Content-Length: %d" st.st_size;
                 sprintf "Date: %s" (Date.asc_now());
                 sprintf "Last-modified: %s" (Date.asc st.st_mtime)
                ];
              document_feed = Feed.of_fd s;
              document_fragment = wr.www_fragment;
              document_logger = Document.tty_logger
            } in
           Retype.f dh;
           cont.document_process dh;
           (fun () -> ())
    end
    | S_DIR -> 
        let s = dir path in
        cont.document_process 
          { document_id = document_id wr;
            document_referer = wr.www_link.h_context;
            document_status = 200;
            dh_headers = ["Content-Type: text/html"];
            document_feed = Feed.of_fd s;
            document_fragment = wr.www_fragment;
            document_logger = Document.tty_logger
           };
         (fun () -> ())

    | _ -> raise (File_error (s_ "cannot open file"))
(*e: function [[File.request]] *)
(*e: protocols/file.ml *)

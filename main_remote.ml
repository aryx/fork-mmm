(*s: ./main_remote.ml *)
(* Talk to an mmm master *)
open Unix

(*s: function Main_remote.request *)
let request sock cmd url =
  if cmd <> "" 
  then write sock cmd 0 (String.length cmd) |> ignore;
  write sock url 0 (String.length url) |> ignore;
  write sock "\n" 0 1 |> ignore;
  let buf = String.create 1024 in
  try
    while true do
      let n = read sock buf 0 1024 in
      if n = 0 
      then raise End_of_file 
      else ignore (write stdout buf 0 n)
    done
  with End_of_file -> close sock
(*e: function Main_remote.request *)
    
(*s: function Main_remote.main *)
let main () =
  let file = 
    Filename.concat (Filename.concat (Sys.getenv "HOME") ".mmm") "remote" in
  let cmd = ref "" in
  
  let s = socket PF_UNIX SOCK_STREAM 0 in
  connect s (ADDR_UNIX file);
  Arg.parse [ 
    "-get", Arg.Unit (fun () -> cmd := "GET "), "Get document";
    "-getbody", Arg.Unit (fun () -> cmd := "GETB "), "Get document body";
    "-head", Arg.Unit (fun () -> cmd := "HEAD "), "Get document headers";
    "-show", Arg.Unit (fun () -> cmd := "DISPLAY "), "Open browser on this URL";
  ]
    (fun url -> request s !cmd url)
    "Usage: mmm_remote [-get | -getbody | -head | -show] <url>\n
     The default is -show."
(*e: function Main_remote.main *)

(*s: toplevel Main_remote._1 *)
let _ = Printexc.catch main ()
(*e: toplevel Main_remote._1 *)
(*e: ./main_remote.ml *)

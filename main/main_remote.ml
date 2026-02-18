(*s: main_remote.ml *)
(* Talk to an mmm master *)

type caps = < Cap.network >

(*s: function [[Main_remote.request]] *)
let request sock (cmd : string) (url : string) =
  if cmd <> "" 
  then Unix.write sock (Bytes.of_string cmd) 0 (String.length cmd) |> ignore;

  Unix.write sock (Bytes.of_string url) 0 (String.length url) |> ignore;
  Unix.write sock (Bytes.of_string "\n") 0 1 |> ignore;
  let buf = Bytes.create 1024 in
  try
    while true do
      let n = Unix.read sock buf 0 1024 in
      if n = 0 
      then raise End_of_file 
      else ignore (Unix.write Unix.stdout buf 0 n)
    done
  with End_of_file -> Unix.close sock
(*e: function [[Main_remote.request]] *)
    
(*s: function [[Main_remote.main]] *)
let main (caps: < caps; Cap.stdout; Cap.stderr; ..>) (argv : string array)
    : Exit.t =
  let file = 
    Filename.concat (Filename.concat (Sys.getenv "HOME") ".mmm") "remote" in
  let cmd = ref "" in
  
  let s = Unix.socket PF_UNIX SOCK_STREAM 0 in
  CapUnix.connect caps s (ADDR_UNIX file);
  Arg_.parse_argv caps argv  [ 
    "-get", Arg.Unit (fun () -> cmd := "GET "), "Get document";
    "-getbody", Arg.Unit (fun () -> cmd := "GETB "), "Get document body";
    "-head", Arg.Unit (fun () -> cmd := "HEAD "), "Get document headers";
    "-show", Arg.Unit (fun () -> cmd := "DISPLAY "), "Open browser on this URL";
  ]
    (fun url -> request s !cmd url)
    "Usage: mmm_remote [-get | -getbody | -head | -show] <url>\n
     The default is -show.";
   Exit.OK
(*e: function [[Main_remote.main]] *)

(*s: toplevel [[Main_remote._1]] *)
let _ = 
  Cap.main (fun (caps : Cap.all_caps) ->
    let argv = CapSys.argv caps in
    Exit.exit caps (Exit.catch (fun () -> main caps argv)))
(*e: toplevel [[Main_remote._1]] *)
(*e: main_remote.ml *)

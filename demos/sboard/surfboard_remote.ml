(* Talk to an mmm master *)
open Unix

let main () =
 let file = 
   Filename.concat (Filename.concat (Sys.getenv "HOME") ".mmm")
		   "surfport" in

  let socket = socket PF_UNIX SOCK_STREAM 0 in
  connect socket (ADDR_UNIX file);
  let args = ref [] in
  Arg.parse []
    (fun s -> args := s :: !args)
    "Usage: surfboard_remote <url> <title>\n";
  args := List.rev !args;
  match !args with
    [url; title] ->
      ignore (write socket url 0 (String.length url));
      ignore (write socket " " 0 1);
      ignore (write socket title 0 (String.length title));
      ignore (write socket "\n" 0 1)
  | _ -> prerr_endline "Usage: surfboard_remote <url> <title>\n"

let _ = Printexc.catch main ()

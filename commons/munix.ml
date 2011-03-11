open Printf
open Unix

(*
 * Simple Unix utilities
 *)


(* If execvp fails in one of our children, it may be dangerous to leave
   the program running, since we don't know how Tk would react *)
let execvp s args =
  try 
    execvp s args
  with
    Unix_error(e, _, _) ->
       Printf.eprintf "%s\n" (Unix.error_message e);
       flush Pervasives.stderr;
       exit 1

let quote = Str.regexp "'"
let quote_for_shell s =
  sprintf "'%s'" (Str.global_replace quote "'\\''" s)

(* Wrapping of Sys.command with trivial arg quoting *)
let system cmd args back =
  let b = Ebuffer.create 128 in
   Ebuffer.output_string b cmd;
   List.iter (fun s ->
     Ebuffer.output_char b ' ';
     Ebuffer.output_string b (quote_for_shell s))
    args;
   if back then Ebuffer.output_string b " &";
   Sys.command (Ebuffer.get b)

let eval_cmd cmd args back =
 let _ = system cmd args back in ()

let write_string fd s =
  ignore (write fd s 0 (String.length s))

(*
 * Read a line (terminated by \n or \r\n).
 *   strips terminator !
 *)
let read_line fd =
  let rec read_rec buf bufsize offs =
    let n = Low.read fd buf offs 1 in
      if n = 0 then raise End_of_file
      else if buf.[offs] = '\n'
           then (* strips \n and possibly \r  *)
             let len = if offs >= 1 & buf.[offs-1] = '\r' then offs-1 
                       else offs in
               String.sub buf 0 len
           else let offs = succ offs in
                  if offs = bufsize 
                  then read_rec (buf ^ String.create 128) (bufsize + 128) offs
                  else read_rec buf bufsize offs in
  read_rec (String.create 128) 128 0 


let full_random_init () =
  try 
    let env = environment () in
    let vect =
      Array.append (Array.map Hashtbl.hash env)
		   [| getpid(); Pervasives.truncate (time()); (* JPF: bogus *)
		      getuid(); getgid();
		      Hashtbl.hash (getlogin()) |] in
    Random.full_init vect
  with
    _ -> ()

let rec digdir dir perm =
  (* try to create the directory dir *)
  if Sys.file_exists dir then () 
  else begin
    let pdir = Filename.dirname dir in
    digdir pdir perm;
    Unix.mkdir dir perm
  end

(* DNS Caching. It really helps on slow lines... *)
let dns = Hashtbl.create 307
let gethostbyname h =
  try Hashtbl.find dns h
  with
    Not_found ->
      let addr = Unix.gethostbyname h in
      	 Hashtbl.add dns h addr;
	 addr

let _ =
  full_random_init()

(* Hack to run some external command with parameter substitution 
 * The command is a string containing $X
 * The arguments are [X, v]
 * For arguments not substituted, add them at the end,
 *)
let vars = Str.regexp "\\$[A-Z]+"

let system_eval cmd args back =
  let replaced = ref []
  and qargs = List.map (fun (x, v) -> x, quote_for_shell v) args
  in
  let replfun s =
    let matched = Str.matched_string s in
    let thevar = String.sub matched 1 (String.length matched - 1) in
    try 
      let res = List.assoc thevar args in
      replaced := thevar :: !replaced;
      res
    with
      Not_found -> matched
  in
  (* replace vars *)
  let scmd = Str.global_substitute vars replfun cmd in
  (* for vars that haven't been replaced, add them at the end 
   * (backward compatibility with our previous versions)
   *)
  let remaining = ref [] in
  List.iter (fun (x,v) -> 
    if not (List.mem x !replaced) then remaining := v :: !remaining)
    args;
  system scmd (List.rev !remaining) back


(***********************************************************************)
(*                                                                     *)
(*                           Calves                                    *)
(*                                                                     *)
(*          Francois Rouaix, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* A PGP decoder *)
open Msys
open Mstring
open Tk
open Unix
(* 
   decoding goes as follows:
    pgp -f +batchmode < signed > unsigned 2> log
   to produce a signed file,
    pgp -s foo # produces foo.pgp
 or pgp -sta foo # produces foo.asc

pgp_check is called on file (saved by a scheduler)
when Content-Encoding was specified as PGP

*)

(* Read on a channel until eof *)
let read_all chan =
  let len = 1024 in
  let rec read_chunk buffer offs = 
    let chunk = read chan buffer offs len in
    if chunk = 0 then String.sub buffer 0 offs else
      let newoffs = offs + chunk
      and l = String.length buffer in
        if newoffs + len > l then
          let newbuf = String.create (2*l+len) in
           String.unsafe_blit buffer 0 newbuf 0 newoffs;
           read_chunk newbuf newoffs
        else
           read_chunk buffer newoffs
  in
  read_chunk (String.create 2048) 0

let batch_pgp signed clear pgplog  =
  let oin = openfile signed [O_RDONLY] 0 
  and oout = openfile clear  [O_WRONLY; O_CREAT] 0o600 in
   dup2 oin stdin; close oin;
   dup2 oout stdout; close oout;
   dup2 pgplog stderr; close pgplog;
  try
    execvp "pgp" [| "pgp"; "+batchmode=on"; "+verbose=0"; "-f" |]
  with
    Unix_error(e, _, _) ->
      Printf.eprintf "%s\n" (Unix.error_message e);
      flush Pervasives.stderr;
      exit 1


let check url signed_file =
 let clear_file = mktemp "clear" in
   let rec attempt () =
     let (lin, lout) = pipe() in
       match Low.fork() with
       	 0 -> close lin;
	      batch_pgp signed_file clear_file lout;
	      None
       | n ->
       	  close lout;
	  let res = read_all lin in
	  close lin;
	  let p, st = waitpid [] n in
	  match st with
	      WEXITED n -> 
	         begin match
		  Frx_dialog.f Widget.default_toplevel (gensym "pgp")
		    "PGP Authentification" 
      	       	    (I18n.sprintf "PGP diagnostic for %s\n%s" url res)
		    (Predefined "question")
		    (if n = 0 then 0 else 1)
		    ["Accept"; "Reject"; "Retry"] with
            	 |  0 -> (* yeah *) Some clear_file
		 | 1 -> (* duh *) Msys.rm clear_file; None
		 | 2 -> attempt ()
                 | _ -> assert false
		 end
	    | _ -> 
	       Msys.rm clear_file;
      	       Error.f "PGP aborted";
	       None
   in
   attempt()





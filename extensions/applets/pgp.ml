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
  let rec read_chunk (buffer : bytes) (offs : int) : string = 
    let chunk = Unix.read chan buffer offs len in
    if chunk = 0 then Bytes.sub_string buffer 0 offs else
      let newoffs = offs + chunk
      and l = Bytes.length buffer in
        if newoffs + len > l then
          let newbuf = Bytes.create (2*l+len) in
           Bytes.unsafe_blit buffer 0 newbuf 0 newoffs;
           read_chunk newbuf newoffs
        else
           read_chunk buffer newoffs
  in
  read_chunk (Bytes.create 2048) 0

let batch_pgp signed clear pgplog  =
  let oin = Unix.openfile signed [O_RDONLY] 0 
  and oout = Unix.openfile clear  [O_WRONLY; O_CREAT] 0o600 in
   Unix.dup2 oin Unix.stdin; Unix.close oin;
   Unix.dup2 oout Unix.stdout; Unix.close oout;
   Unix.dup2 pgplog Unix.stderr; Unix.close pgplog;
  try
    Unix.execvp "pgp" [| "pgp"; "+batchmode=on"; "+verbose=0"; "-f" |]
  with
    Unix.Unix_error(e, _, _) ->
      Printf.eprintf "%s\n" (Unix.error_message e);
      flush Stdlib.stderr;
      exit 1


let check url signed_file =
 let clear_file = mktemp "clear" in
   let rec attempt () =
     let (lin, lout) = Unix.pipe() in
       match Low.fork() with
       	 0 -> Unix.close lin;
	      batch_pgp signed_file clear_file lout
               (* never reached *)
	      (* old: None *)
       | n ->
       	  Unix.close lout;
	  let res = read_all lin in
	  Unix.close lin;
	  let _p, st = Unix.waitpid [] n in
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





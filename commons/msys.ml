(*s: commons/msys.ml *)
(*s: copyright header v6 *)
(***********************************************************************)
(*                                                                     *)
(*                           The V6 Engine                             *)
(*                                                                     *)
(*          Francois Rouaix, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header v6 *)

open Printf
open Unix

(* Tilde substitution *)

(*s: function [[Msys.next_slash]] *)
(* skip to next / *)
let rec next_slash s n =
  if  n >= String.length s || s.[n] = '/' 
  then n
  else next_slash s (succ n)
(*e: function [[Msys.next_slash]] *)

(*s: function [[Msys.tilde_subst]] *)
let tilde_subst s =
 try
  if s = "" || s.[0] <> '~' then s 
  else
    let len = String.length s in
    if len = 1 then Sys.getenv "HOME"
    else match s.[1] with
      '/' -> 
        Filename.concat (Sys.getenv "HOME") (String.sub s 2 (len - 2))
     | _ ->
       let final = next_slash s 1 in
       let user = String.sub s 1 (pred final) in
       let pwnam = getpwnam user in
         if succ final >= len then pwnam.pw_dir
         else
          Filename.concat pwnam.pw_dir 
               (String.sub s (succ final) (len - (succ final)))
 with
    Unix_error(_,_,_) -> s
  | Sys_error _ -> s
  | Not_found -> s
(*e: function [[Msys.tilde_subst]] *)

(*s: function [[Msys.rm]] *)
(* Quiet unlink *)
let rm s = try unlink s with Unix_error _ -> ()
(*e: function [[Msys.rm]] *)
(*s: function [[Msys.rmdir]] *)
let rmdir dir =
  try
    let dh = opendir dir 
    and l = ref [] in
    try while true do
      let f = readdir dh in
      if f <> "." && f <> ".." then l := f :: !l
    done
    with
      End_of_file -> 
    closedir dh;
    List.iter (fun f -> rm (Filename.concat dir f)) !l;
    Unix.rmdir dir
  with
    Unix_error _ -> ()
(*e: function [[Msys.rmdir]] *)

(*s: function [[Msys.fsize]] *)
let fsize f =
  try (Unix.stat f).st_size
  with Unix_error(_,_,_) -> raise Not_found
(*e: function [[Msys.fsize]] *)

(*s: constant [[Msys.tmp_dir]] *)
let tmp_dir = ref "/tmp"
(*e: constant [[Msys.tmp_dir]] *)

(*s: constant [[Msys.mktemp]] *)
(* We know use our own private directory in /tmp, cleared at exit-time,
   so no one can snoop our temporary files *)
let mktemp =
  let cnter = ref 0 
  and pid = Unix.getpid() 
  and id = ref 0 in
  let thisdir = 
    let testdir = ref "" in
    try while true do
      testdir := Filename.concat !tmp_dir ("mmm" ^ string_of_int pid
                         ^ "_" ^ string_of_int !id);
      if not (Sys.file_exists !testdir) then raise Exit;
      incr id;
      if !id >= 16 then 
    raise (Failure ("Too many MMM temporary directory in " ^ !tmp_dir ^
            ". Clean them first."))
    done; "" (* cannot reach *)
    with
      Exit -> !testdir
  in
  Unix.mkdir thisdir 0o700;
  at_exit (fun () -> rmdir thisdir);
  (function prefx -> 
      incr cnter; 
      (Filename.concat thisdir (prefx ^ string_of_int !cnter)))
(*e: constant [[Msys.mktemp]] *)
(*e: commons/msys.ml *)

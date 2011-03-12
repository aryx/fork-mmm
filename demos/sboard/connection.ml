open Unix
open Tk
open Tree

let client_navigator url = (* for mmm *)
  let file = 
    Misc.user_file ".mmm/remote"
  in
  let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.connect socket (Unix.ADDR_UNIX file);
  ignore (Unix.write socket url 0 (String.length url));
  ignore (Unix.write socket "\n" 0 1);
  Unix.close socket


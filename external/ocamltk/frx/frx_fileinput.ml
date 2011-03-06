open Tk

let version = "$Id: frx_fileinput.ml,v 1.1 1996/10/22 15:55:29 rouaix Exp $"

(*
 * Simple spooling for fileinput callbacks
 *)

let waiting_list = Queue. new()
and waiting = ref 0
and max_open = ref 10
and cur_open = ref 0

let add fd f =
  if !cur_open < !max_open then begin
    incr cur_open;
    add_fileinput fd f
    end
  else begin
    incr waiting;
    Queue.add (fd,f) waiting_list
  end

let remove fd =
  

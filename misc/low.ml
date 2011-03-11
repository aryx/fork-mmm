(* Wrapping of some low-level functions *)

open Unix
open Tk

(* Tachymeter support *)
class  virtual tachymeter (w : Widget.widget) =
 object
  method virtual report_cnx : int -> unit     (* displays number of active cnx *)
  method virtual report_busy : bool -> unit   (* displays busy status *)
  method virtual report_traffic : int -> int -> int -> unit
       (* [report_traffic tick_duration total sample] displays traffic
          from [total] and [sample] in last [tick_duration] *)
  method virtual quit : unit
  end

class no_tachy (w : Widget.widget) =
 object
  inherit tachymeter (w)
  method report_cnx cnx = ()
  method report_busy flag = ()
  method report_traffic tick total sample = ()
  method quit = ()

  end

let cur_tachy = ref (new no_tachy Widget.default_toplevel :> tachymeter)

(* for the tachymeter *)
let bytes_read = ref 0
and sample_read = ref 0

let read fd buf offs l =
  let n = Unix.read fd buf offs l in
    bytes_read := !bytes_read + n;
    sample_read := !sample_read + n;
    n

let pending_read = ref 0
and action = ref (fun _ -> ())

let add_fileinput fd f =
  incr pending_read;
  !cur_tachy#report_cnx !pending_read;
  Fileevent.add_fileinput fd f

let remove_fileinput fd =
  decr pending_read;
  !cur_tachy#report_cnx !pending_read;
  Fileevent.remove_fileinput fd

(* We catch dead children here, to avoid large number of zombies.
   I know about SICHLD of course, but I hate interrupted syscalls
 *)

external sys_exit : int -> 'a = "caml_sys_exit"

let fork () =
 begin try
  while 
    let p, s = Unix.waitpid [Unix.WNOHANG] 0 in 
      (*
      Printf.eprintf "%d\n" p;
      begin match s with
      	 WEXITED n -> Printf.eprintf "Exit %d\n" n
       | WSIGNALED(n,_) -> 
      	    Printf.eprintf "SIG %d\n" n
       | WSTOPPED n -> Printf.eprintf "Stopped %d\n" n
      end;
      flush Pervasives.stderr;
      *)
      p <> 0
 do () done
 with
  Unix.Unix_error(_,_,_) -> ()
 end;
 (* Don't let children play stupid games *)
 match Unix.fork() with
   0 -> at_exit (fun () -> sys_exit 0); 0
 | n -> n


let busy f x =
  !cur_tachy#report_busy true;
  try 
    let v = f x in
    !cur_tachy#report_busy false; v
  with
    e ->
      !cur_tachy#report_busy false;
      raise e

let global_time = ref 0
let tick_duration = 500

(* There is only a default task *)
let tasks = ref [
  (fun () -> !cur_tachy#report_traffic tick_duration !bytes_read !sample_read)
  ]

let rec refresh() =
  incr global_time;
  List.iter (fun f -> f ()) !tasks;
  sample_read := 0;
  let _ = Timer.add tick_duration refresh in ()

let add_task f = tasks := f :: !tasks

let init () = refresh ()

(* We need manual refresh for progressive display (?), but we don't
   want to do it too frequently *)
let last_update = ref !global_time
let update_idletasks () =
  if !global_time <> !last_update then begin
    update_idletasks(); last_update := !global_time
  end

(*s: ./commons/low.ml *)
(* Wrapping of some low-level functions *)
open Common

open Unix

(* Tachymeter support *)
class  virtual tachymeter = object
  method virtual report_cnx : int -> unit     (* displays number of active cnx *)
  method virtual report_busy : bool -> unit   (* displays busy status *)
  method virtual report_traffic : int -> int -> int -> unit
       (* [report_traffic tick_duration total sample] displays traffic
          from [total] and [sample] in last [tick_duration] *)
  method virtual quit : unit
  end

class no_tachy = object
  inherit tachymeter

  method report_cnx cnx = ()
  method report_busy flag = ()
  method report_traffic tick total sample = ()
  method quit = ()
end

(*s: constant Low.cur_tachy *)
let cur_tachy = ref (new no_tachy :> tachymeter)
(*e: constant Low.cur_tachy *)

(* for the tachymeter *)
let bytes_read = ref 0
and sample_read = ref 0

(*s: function Low.read *)
let read fd buf offs l =
  let n = Unix.read fd buf offs l in
    bytes_read := !bytes_read + n;
    sample_read := !sample_read + n;
    n
(*e: function Low.read *)

let pending_read = ref 0
and action = ref (fun _ -> ())

(*s: function Low.add_fileinput *)
let add_fileinput fd f =
  incr pending_read;
  !cur_tachy#report_cnx !pending_read;
  Fileevent_.add_fileinput fd f
(*e: function Low.add_fileinput *)

(*s: function Low.remove_fileinput *)
let remove_fileinput fd =
  decr pending_read;
  !cur_tachy#report_cnx !pending_read;
  Fileevent_.remove_fileinput fd
(*e: function Low.remove_fileinput *)

(* We catch dead children here, to avoid large number of zombies.
   I know about SICHLD of course, but I hate interrupted syscalls
 *)

external sys_exit : int -> 'a = "caml_sys_exit"

(*s: function Low.fork *)
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
(*e: function Low.fork *)


(*s: function Low.busy *)
let busy f x =
  !cur_tachy#report_busy true;
  try 
    let v = f x in
    !cur_tachy#report_busy false; v
  with
    e ->
      !cur_tachy#report_busy false;
      raise e
(*e: function Low.busy *)

(*s: constant Low.global_time *)
let global_time = ref 0
(*e: constant Low.global_time *)
(*s: constant Low.tick_duration *)
let tick_duration = 500
(*e: constant Low.tick_duration *)

(*s: constant Low.tasks *)
(* There is only a default task *)
let tasks = ref [
  (fun () -> !cur_tachy#report_traffic tick_duration !bytes_read !sample_read)
  ]
(*e: constant Low.tasks *)

let timer_add_backend = 
  ref (fun _ _ -> failwith "no time_add defined")

(*s: function Low.refresh *)
let rec refresh() =
  incr global_time;
  List.iter (fun f -> f ()) !tasks;
  sample_read := 0;
  !timer_add_backend tick_duration refresh
(*e: function Low.refresh *)

(*s: function Low.add_task *)
let add_task f = tasks := f :: !tasks
(*e: function Low.add_task *)

(*s: function Low.init *)
let init () = refresh ()
(*e: function Low.init *)

let update_idletasks_backend = 
  ref (fun _ -> failwith "no update_idletasks defined")


(*s: constant Low.last_update *)
(* We need manual refresh for progressive display (?), but we don't
   want to do it too frequently *)
let last_update = ref !global_time
(*e: constant Low.last_update *)
(*s: function Low.update_idletasks *)
let update_idletasks () =
  if !global_time <> !last_update then begin
    !update_idletasks_backend ();
    last_update := !global_time
  end
(*e: function Low.update_idletasks *)
(*e: ./commons/low.ml *)

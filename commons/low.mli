(*s: ./commons/low.mli *)
open Unix

(*s: signature Low.read *)
val read : file_descr -> string -> int -> int -> int
  (* Unix.read wrapper, to be used when data transferred has to
     be counted by the tachymeter
   *)
(*e: signature Low.read *)

(*s: signature Low.fork *)
val fork : unit -> int
  (* Unix.fork wrapper. Catches zombies *)
(*e: signature Low.fork *)

(*s: signature Low.add_fileinput *)
val add_fileinput : file_descr -> (unit -> unit) -> unit
(*e: signature Low.add_fileinput *)
(*s: signature Low.remove_fileinput *)
val remove_fileinput: file_descr -> unit
  (* Wrapping of Tk fileinput functions, with feedback on the tachymeter *)
(*e: signature Low.remove_fileinput *)

(*s: signature Low.busy *)
val busy : ('a -> 'b) -> 'a -> 'b
  (* Busy feedback during this application *)
(*e: signature Low.busy *)

(*s: signature Low.global_time *)
val global_time : int ref
(*e: signature Low.global_time *)
(*s: signature Low.add_task *)
val add_task : (unit -> unit) -> unit
  (* regular tasks *)
(*e: signature Low.add_task *)


class  virtual tachymeter : object
  method virtual report_cnx : int -> unit     (* displays number of active cnx *)
  method virtual report_busy : bool -> unit   (* displays busy status *)
  method virtual report_traffic : int -> int -> int -> unit
       (* [report_traffic tick_duration total sample] displays traffic
          from [total] and [sample] in last [tick_duration] *)
  method virtual quit : unit
end

(*s: signature Low.cur_tachy *)
val cur_tachy : tachymeter ref
(*e: signature Low.cur_tachy *)


(*s: signature Low.init *)
val init : unit -> unit
(*e: signature Low.init *)


val update_idletasks_backend: (unit -> unit) ref

(*s: signature Low.update_idletasks *)
val update_idletasks : unit -> unit
(*e: signature Low.update_idletasks *)
(*e: ./commons/low.mli *)

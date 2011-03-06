open Unix

val read : file_descr -> string -> int -> int -> int
  (* Unix.read wrapper, to be used when data transferred has to
     be counted by the tachymeter
   *)

val fork : unit -> int
  (* Unix.fork wrapper. Catches zombies *)

val add_fileinput : file_descr -> (unit -> unit) -> unit
val remove_fileinput: file_descr -> unit
  (* Wrapping of Tk fileinput functions, with feedback on the tachymeter *)

val busy : ('a -> 'b) -> 'a -> 'b
  (* Busy feedback during this application *)

val global_time : int ref
val add_task : (unit -> unit) -> unit
  (* regular tasks *)


class  virtual tachymeter : (Widget.widget) -> object
  method virtual report_cnx : int -> unit     (* displays number of active cnx *)
  method virtual report_busy : bool -> unit   (* displays busy status *)
  method virtual report_traffic : int -> int -> int -> unit
       (* [report_traffic tick_duration total sample] displays traffic
          from [total] and [sample] in last [tick_duration] *)
  method virtual quit : unit
  end

val cur_tachy : tachymeter ref


val init : unit -> unit

val update_idletasks : unit -> unit

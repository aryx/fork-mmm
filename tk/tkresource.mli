val readfile : string -> Tk.optionPriority -> unit
    (* wrapper for Resource.readfile *)

val int :  string -> int -> int
val string : string -> string -> string
val stringlist : string -> string list -> string list
val relief : string -> Tk.relief -> Tk.relief

val event_sequence : string -> (Tk.modifier list * Tk.xEvent) list -> 
                               (Tk.modifier list * Tk.xEvent) list 

val pp_event_sequence : (Tk.modifier list * Tk.xEvent) list -> string
val short_event_sequence : (Tk.modifier list * Tk.xEvent) list -> string

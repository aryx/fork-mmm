open Unix

val   add_fileinput : file_descr -> (unit -> unit) -> unit
val   remove_fileinput: file_descr -> unit
val   add_fileoutput : file_descr -> (unit -> unit) -> unit
val   remove_fileoutput: file_descr -> unit
      (* see [tk] module *)

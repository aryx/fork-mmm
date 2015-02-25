open Common

let (add_fileinput : Unix.file_descr -> (unit -> unit) -> unit) =
 fun _ _ -> 
    pr2 "TODO: File_event.add_fileinput"


let (remove_fileinput: Unix.file_descr -> unit) =
 fun _ ->
    pr2 "TODO: File_event.remove_fileinput"

let (add_fileoutput : Unix.file_descr -> (unit -> unit) -> unit) =
 fun _ _ ->
    pr2 "TODO: File_event.add_fileoutput"

let (remove_fileoutput: Unix.file_descr -> unit) =
 fun _ ->
    pr2 "TODO: File_event.remove_fileoutput"

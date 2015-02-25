open Common

let add_fileinput_ref = ref (fun _ _ ->
    pr2 "TODO: File_event.add_fileinput"
)
let remove_fileinput_ref = ref (fun _ ->
    pr2 "TODO: File_event.remove_fileinput"
)
let add_fileoutput_ref = ref (fun _ _ ->
    pr2 "TODO: File_event.add_fileoutput"
)
let remove_fileoutput_ref = ref (fun _ ->
    pr2 "TODO: File_event.remove_fileoutput"
)

let (add_fileinput : Unix.file_descr -> (unit -> unit) -> unit) =
 fun a b -> !add_fileinput_ref a b


let (remove_fileinput: Unix.file_descr -> unit) =
 fun a ->
   !remove_fileinput_ref a

let (add_fileoutput : Unix.file_descr -> (unit -> unit) -> unit) =
 fun a b ->
   !add_fileoutput_ref a b

let (remove_fileoutput: Unix.file_descr -> unit) =
 fun a ->
   !remove_fileoutput_ref a

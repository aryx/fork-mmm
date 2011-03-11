(* file: protocol *)
val request :  Www.request -> Document.document_continuation -> unit -> unit
    (* [request wr cont] returns [abort] *)
exception File_error of string

val pref_init : Textvariable.textVariable -> unit
val pref_set  : Textvariable.textVariable -> unit

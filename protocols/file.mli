(*s: ./protocols/file.mli *)
(*s: signature File.request *)
(* file: protocol *)
val request :  Www.request -> Document.document_continuation -> unit -> unit
    (* [request wr cont] returns [abort] *)
(*e: signature File.request *)
(*s: exception File.File_error *)
exception File_error of string
(*e: exception File.File_error *)

(*s: signature File.pref_init *)
val pref_init : Textvariable.textVariable -> unit
(*e: signature File.pref_init *)
(*s: signature File.pref_set *)
val pref_set  : Textvariable.textVariable -> unit
(*e: signature File.pref_set *)
(*e: ./protocols/file.mli *)

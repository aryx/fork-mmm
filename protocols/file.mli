(*s: ./protocols/file.mli *)
(*s: signature File.request *)
(* file: protocol *)
val request : Www.request -> Document.document_continuation -> Www.aborter
    (* [request wr cont] returns [abort] *)
(*e: signature File.request *)
(*s: exception File.File_error *)
exception File_error of string
(*e: exception File.File_error *)

(* pad: for tk_file.ml *)
val binary_path: string list ref

(*s: signature File.pref_init *)
(*e: signature File.pref_init *)
(*s: signature File.pref_set *)
(*e: signature File.pref_set *)
(*e: ./protocols/file.mli *)

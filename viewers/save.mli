(*s: viewers/save.mli *)
(*s: signature [[Save.interactive]] *)
val interactive : (string -> unit) -> Document.handle -> unit
(*e: signature [[Save.interactive]] *)
(*s: signature [[Save.transfer]] *)
val transfer : Www.request -> Document.handle -> (Unix.file_descr * bool) option -> unit
(*e: signature [[Save.transfer]] *)
(*s: signature [[Save.tofile]] *)
val tofile : (string -> unit) -> Document.handle -> string -> string -> unit
(*e: signature [[Save.tofile]] *)


(*s: signature [[Save.document]] *)
val document : Document.document_id -> string option -> unit
(*e: signature [[Save.document]] *)
(*s: signature [[Save.print_command]] *)
val print_command : string ref
(*e: signature [[Save.print_command]] *)
(*e: viewers/save.mli *)

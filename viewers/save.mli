val interactive : (string -> unit) -> Document.handle -> unit
val transfer : Www.request -> Document.handle -> (Unix.file_descr * bool) option -> unit
val tofile : (string -> unit) -> Document.handle -> string -> string -> unit


val document : Document.document_id -> string option -> unit
val print_command : string ref

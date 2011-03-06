(* Various dialog boxes *)
val open_simple :
  string ->
  (string -> unit) -> (unit -> 'a) -> Textvariable.textVariable -> unit
 (* [open_simple title action cancelled memory]
    A dialog with a message and an entry field (with memory between
    invocations). Either [action] or [cancelled] is called when the user
    answers to the dialog (with Ok or Cancel)
  *)

val open_simple_synchronous : string -> Textvariable.textVariable -> bool
 (* [open_simple_synchronous title memory]
    A synchronous dialog with a message and an entry field (with 
    memory between invocations). Returns true if the user clicks Ok
    or false if the user clicks Cancel.
  *)
val open_list :
  string -> string list -> (string -> unit) -> (unit -> unit) -> unit
 (* [open_list title elements action cancelled]
    A dialog for selecting from a list of elements. [action] is called
    on each selected element, or [cancelled] is called if the user clicks
    Cancel.
  *)

val open_passwd : string -> string * string
 (* [open_passwd title] pops up a username/password dialog and returns
    (username, password).
  *)

module Frx_misc : sig
  val create_photo : Tk.options list -> Tk.imagePhoto
end

module Frx_req : sig
  val open_simple : 
    string -> (string -> unit) -> (unit -> unit) -> Textvariable.textVariable 
    -> unit
  val open_simple_synchronous : string -> Textvariable.textVariable -> bool
  val open_list :
    string -> string list -> (string -> unit) -> (unit -> unit) -> unit
  val open_passwd : string -> (string * string)
  end

module Frx_dialog: sig
  val f : 
   Widget.widget -> string -> string -> string -> Tk.bitmap -> int -> string list -> int
  end


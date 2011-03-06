val f :
  Widget.widget ->
  string -> string -> string -> Tk.bitmap -> int -> string list -> int
  (* same as Dialog.create_named, but with a local variable for 
     synchronisation. Makes it possible to have several dialogs 
     simultaneously *)

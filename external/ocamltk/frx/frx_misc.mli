val autodef : (unit -> 'a) -> (unit -> 'a)
  (* [autodef make] is a pleasant wrapper around 'a option ref *)

val create_photo : Tk.options list -> Tk.imagePhoto
  (* [create_photo options] allows Data in options (by saving to tmp file) *)

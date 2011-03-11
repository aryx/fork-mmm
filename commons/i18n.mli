val message_file : string ref
val language : string ref

val sprintf: ('a, unit, string) format -> 'a

val menu_option: Tk.options list -> Tk.options list
val menu_pattern: Tk.options list -> string

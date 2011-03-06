val abs_index : int -> Tk.textIndex
  (* [abs_index offs] returns the corresponding TextIndex *)

val insertMark : Tk.textIndex
val currentMark : Tk.textIndex
val textEnd : Tk.textIndex
val textBegin : Tk.textIndex
  (* shortcuts for various positions in a text widget *)

val scroll_link : Widget.widget -> Widget.widget -> unit
  (* [scroll_link scrollbar text] links a scrollbar and a text widget
     as expected
   *)

val new_scrollable_text :
  Widget.widget -> Tk.options list -> bool -> Widget.widget * Widget.widget
  (* [new_scrollable_text parent opts nav_keys] makes a scrollable text
     widget with optional navigation keys. Returns frame and text widget.
   *)
val addsearch : Widget.widget -> unit
  (* [addsearch textw] adds a search dialog bound on [Control-s]
     on the text widget
   *)

val navigation_keys : Widget.widget -> unit
  (* [navigation_keys textw] adds common navigations functions to [textw] *)

val init : unit -> unit
  (* [init ()] must be called before any of the above features is used *)

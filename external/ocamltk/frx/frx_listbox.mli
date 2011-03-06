val scroll_link : Widget.widget -> Widget.widget -> unit
  (* [scroll_link scrollbar listbox] links [scrollbar] and [listbox]
     as expected.
   *)

val add_completion : Widget.widget -> (Tk.eventInfo -> unit) -> unit
  (* [add_completion listbox action] adds Macintosh like electric navigation
     in the listbox when characters are typed in.
     [action] is invoked if Return is pressed
   *)

val new_scrollable_listbox :
  Widget.widget -> Tk.options list -> Widget.widget * Widget.widget
  (* [new_scrollable_listbox parent options] makes a scrollable listbox and
     returns (frame, listbox)
   *)

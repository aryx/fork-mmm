val new_label_entry :
  Widget.widget ->
  string -> (string -> unit) -> Widget.widget * Widget.widget
   (* [new_label_entry parent label action]
      creates a "labelled" entry widget where [action] will be invoked
      when the user types Return in the widget.
      Returns (frame widget, entry widget)
    *)
val new_labelm_entry :
  Widget.widget ->
  string -> Textvariable.textVariable -> Widget.widget * Widget.widget
   (* [new_labelm_entry parent label variable]
      creates a "labelled" entry widget whose contents is [variable].
      Returns (frame widget, entry widget)
    *)

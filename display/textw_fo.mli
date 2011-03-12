
val html_bg : string ref
val html_fg : string ref

val usecolors : bool ref
val internal_buffer : int ref

val create :
  (unit -> string) ->
  Htmlfmt.formatterSpec -> Widget.widget -> Viewers.context ->  
    Htmlfmt.formatter * Widget.widget

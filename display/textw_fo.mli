(*s: ./display/textw_fo.mli *)

(*s: signature Textw_fo.html_bg *)
val html_bg : string ref
(*e: signature Textw_fo.html_bg *)
(*s: signature Textw_fo.html_fg *)
val html_fg : string ref
(*e: signature Textw_fo.html_fg *)

(*s: signature Textw_fo.usecolors *)
val usecolors : bool ref
(*e: signature Textw_fo.usecolors *)
(*s: signature Textw_fo.internal_buffer *)
val internal_buffer : int ref
(*e: signature Textw_fo.internal_buffer *)

(*s: signature Textw_fo.create *)
val create :
  (unit -> string) ->
  Htmlfmt.formatterSpec -> 
  Widget.widget -> 
  Viewers.context ->  
  Htmlfmt.formatter * Widget.widget
(*e: signature Textw_fo.create *)
(*e: ./display/textw_fo.mli *)

(*s: display/source.mli *)

(*s: signature [[Source.annotate]] *)
val annotate: 
  Widget.widget -> (Tk.textTag * Html.location) list -> unit
(*e: signature [[Source.annotate]] *)

(*s: signature [[Source.view]] *)
val view:
  Widget.widget ->
  Document.id ->
  (unit -> unit) ->
  (Html.location * string) list ref ->
  (Tk.textTag * Html.location) list ref -> 
  Charset.detected_code ->
  unit
(*e: signature [[Source.view]] *)
(*e: display/source.mli *)

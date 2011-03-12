
val annotate: 
  Widget.widget -> (Tk.textTag * Html.location) list -> unit

val view:
  Widget.widget ->
  Document.document_id ->
  (unit -> unit) ->
  (Html.location * string) list ref ->
  (Tk.textTag * Html.location) list ref -> Japan.detected_code -> unit

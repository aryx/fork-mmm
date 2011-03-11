(* Cache by "widget unmapping"
 *  For each navigator, we keep the list of displayed documents
 *)

val debug : bool ref

val max_keep : int ref

val kill : int -> unit
  (* [kill hkey] destroy all widget cached for navigator [hkey]
     If in history mode, accordingly remove from Cache documents
     that are not shared with other navigator windows
   *)

val find : int -> Document.document_id -> Viewers.display_info
val add : int -> Document.document_id -> Viewers.display_info -> unit
val remove : int -> Document.document_id -> unit
val displace : int -> Document.document_id -> unit

val postmortem : unit -> unit


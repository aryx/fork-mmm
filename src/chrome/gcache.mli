(*s: gui/gcache.mli *)
(* Cache by "widget unmapping"
 * For each navigator, we keep the list of displayed documents
 *)

(*s: signature [[Gcache.debug]] *)
val debug : bool ref
(*e: signature [[Gcache.debug]] *)

(*s: signature [[Gcache.max_keep]] *)
val max_keep : int ref
(*e: signature [[Gcache.max_keep]] *)

(*s: signature [[Gcache.kill]] *)
val kill : int -> unit
  (* [kill hkey] destroy all widget cached for navigator [hkey]
     If in history mode, accordingly remove from Cache documents
     that are not shared with other navigator windows
   *)
(*e: signature [[Gcache.kill]] *)

(*s: signature [[Gcache.find]] *)
val find : int -> Document.id -> Viewers.display_info
(*e: signature [[Gcache.find]] *)
(*s: signature [[Gcache.add]] *)
val add : int -> Document.id -> Viewers.display_info -> unit
(*e: signature [[Gcache.add]] *)
(*s: signature [[Gcache.remove]] *)
val remove : int -> Document.id -> unit
(*e: signature [[Gcache.remove]] *)
(*s: signature [[Gcache.displace]] *)
val displace : int -> Document.id -> unit
(*e: signature [[Gcache.displace]] *)

(*s: signature [[Gcache.postmortem]] *)
val postmortem : unit -> unit
(*e: signature [[Gcache.postmortem]] *)

(*e: gui/gcache.mli *)

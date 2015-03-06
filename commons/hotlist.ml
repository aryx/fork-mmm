(*s: ./commons/hotlist.ml *)
open I18n
(*s: constant Hotlist.program *)
(* A cool module *)
let program = ref ""
(*e: constant Hotlist.program *)

(*s: function Hotlist.f *)
let f url title =
  match !program with 
  | "" -> (!Error.default)#f (s_ "No hotlist command defined")
  | s -> 
      let _ = Munix.system_eval s ["URL", url; "TITLE", title] true in
      (!Error.default)#ok
        (s_ "%s\nadded to hotlist with title\n%s" url title)
(*e: function Hotlist.f *)
(*e: ./commons/hotlist.ml *)

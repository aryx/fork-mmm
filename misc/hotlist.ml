(* A cool module *)
let program = ref ""

let f url title =
  match !program with 
    "" -> Error.default#f (I18n.sprintf "No hotlist command defined")
  | s -> 
      let _ = Munix.system_eval s ["URL", url; "TITLE", title] true in
      Error.default#ok
	(I18n.sprintf "%s\nadded to hotlist with title\n%s" url title)

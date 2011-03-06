(* A selection handler *)
open Widget
open Protocol
open Tk

let frame = ref None
let selection = ref ""

let read ofs n =
  let res =
    if ofs < 0 then ""
    else if ofs + n > String.length !selection
    then String.sub !selection ofs (String.length !selection - ofs)
    else String.sub !selection ofs n in
   tkreturn res

(* As long as we don't loose the selection, we keep the widget *)
(* Calling this function means that we own the selection       *)
(* When we loose the selection, both cb are destroyed *)
let own () =
  match !frame with
    None ->
      let f = Frame.create_named Widget.default_toplevel "frx_selection" [] in
       let lost () = selection := ""; destroy f; frame := None in
       Selection.own_set [Selection "PRIMARY"; LostCommand lost] f;
       Selection.handle_set [Selection "PRIMARY"; ICCCMType "STRING"] f read;
       frame := Some f
  | Some f -> ()

let set s = own(); selection := s

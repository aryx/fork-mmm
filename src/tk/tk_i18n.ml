open Tk
open I18n

(*
val menu_option: Tk.options list -> Tk.options list
val menu_pattern: Tk.options list -> string
*)

let menu_option l =
  let under_pos = ref (-1) 
  and text = ref ""
  in
    List.iter (function 
    UnderlinedChar x -> under_pos := x
      | Text x  -> text := x
      | Label x -> text := x
      |	_ -> () ) l;
    let trans = translate !text in
      let new_text, new_under_pos =
    if !text = trans then !text, !under_pos
    else
      if !under_pos = 0 &&
        String.get !text 0 = String.get trans 0 then trans, 0
      else
        (String.make 1 (String.get !text !under_pos)) ^ ":" ^ trans, 0 
      in
        List.map (function
        UnderlinedChar _ -> UnderlinedChar new_under_pos
      | Text _x -> Text new_text
      | Label _x -> Label new_text
      | x -> x ) l

exception Found of string

let menu_pattern l =
  let l' = menu_option l in
  try
    List.iter (function
      | Text x -> raise (Found x)
      | Label x -> raise (Found x)
      | _ -> ()) l';
    raise (Failure "I18n.menu_pattern : the option list contains no text")
  with
    Found x -> x

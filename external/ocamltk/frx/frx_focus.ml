open Tk

(* Temporary focus *)

(* ? use bind tag ? how about the global reference then *)
let auto w =
  let old_focus = ref w in
  bind w [[],Enter] 
      (BindSet([], fun _ -> old_focus := Focus.get (); Focus.set w));
  bind w [[],Leave] 
      (BindSet([], fun _ -> Focus.set !old_focus))

open Mstring

class t (top) =
 object
 (* val top = top *)

method f msg =
  let _ =
   Frx_dialog.f top (gensym "error")
     (I18n.sprintf "MMM Error")
     msg
     (Predefined "error") 0 ["Ok"] in
  ()


method ok msg = 
  ignore (
   Frx_dialog.f  top (gensym "error")
     (I18n.sprintf "MMM Notify")
     msg
     (Predefined "info") 0 ["Ok"])

method choose msg =
 0 =
  Frx_dialog.f top (gensym "error")
    (I18n.sprintf "MMM Choice")
    msg
    (Predefined "question") 1 ["Yes"; "No"]

method ari msg =
  Frx_dialog.f top (gensym "error")
    (I18n.sprintf "MMM Error")
    msg
    (Predefined "question") 1 
    [I18n.sprintf "A)bort"; I18n.sprintf "R)etry"; I18n.sprintf "I)gnore"]

end

(*TODO put in main.ml? let default = ref new t default_toplevel *)

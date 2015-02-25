(*s: ./commons/error.ml *)
open Tk
open Widget
open Mstring

(*s: class Error.t *)
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
(*e: class Error.t *)

(*s: constant Error.default *)
let default = new t default_toplevel
(*e: constant Error.default *)

(* backward compatibility *)
let f msg = default#f msg
and ok msg = default#ok msg
and choose msg = default#choose msg
and ari msg = default#ari msg
(*e: ./commons/error.ml *)

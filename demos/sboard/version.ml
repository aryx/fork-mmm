open Tk

let about = 
"Surfboard Version 1.0
Copyright by Jun P. Furuse

Jun.Furuse@inria.fr
http://pauillac.inria.fr/~furuse/
"
let about () =
  ignore (
    Frx_dialog.f Widget.default_toplevel (Mstring.gensym "about")
     "About Surfboard" about (Predefined "info") 0 ["Thanks"])

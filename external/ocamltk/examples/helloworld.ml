open Tk;;            (* Make interface functions available *)

let top = openTk ();;   (* Initialisation of the interface *)
(* top is now the toplevel widget *)

(* Widget initialisation *)
let b = Button.create top 
          [Text "foobar"; 
           Command (function () -> 
                      print_string "foobar"; 
                      print_newline();
                      flush stdout)];;
(* b exists but is not yet visible *)

let q = Button.create top 
          [Text "quit"; 
           Command closeTk];;
(* q exists but is not yet visible *)

pack [b; q][] ;;           (* Make b visible *)
mainLoop() ;;           (* User interaction*)
(* You can quit this program by deleting its main window *)

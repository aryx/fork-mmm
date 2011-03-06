(* The code run on initialisation, in addition to normal Tk code
 * NOTE: camltk has not fully been initialised yet
 *)
external tcl_eval : string -> string
      	=  "camltk_tcl_eval"
let tcl_command s = ignore (tcl_eval s);;
open Printf

let dynload args =
  List.iter Dynlink.loadfile args

(* Default modules include everything from 
let default_modules = []

(* [caml::run foo.cmo .. bar.cmo] is now available from Tcl *)
let init () =
  Dynlink.init();
  (* Make it unsafe by default, with everything available *)
  Dynlink.allow_unsafe_modules true;
  Dynlink.add_interfaces [] []
  let s = register_callback Widget.dummy dynload in
  tcl_command (sprintf "proc caml::run {l} {camlcb %s l}" s)

let _ =
  Printexc.print init ()

(* A typical master program would then
 *   caml::run foo.cmo
 *     # during initialisation, "foo" was registered as a tcl procedure
 *   foo x y z
 *     # proceed with some Tcl code calling foo
 *)

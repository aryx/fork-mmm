open Tk
open Protocol
let active_cb _ =
  let cnter = ref 0 in
  Hashtbl.iter 
    (fun w id ->
      incr cnter;
      Printf.fprintf stdout "%s %s %b\n"
      	 (Widget.name w) (string_of_cbid id) (Winfo.exists w))
    callback_memo_table;
  Printf.fprintf stdout "Memo cb: %d\n" !cnter;
  cnter := 0;
  Hashtbl.iter (fun _ _ -> incr cnter) callback_naming_table;
  Printf.fprintf stdout "Active cb: %d\n" !cnter;
  flush stdout

let init () =
  Frx_rpc.register "cb" active_cb;
  Frx_rpc.register "cache"
     (fun _ -> Cache.postmortem(); Gcache.postmortem(); flush stderr);
  Frx_rpc.register "images" (fun _ -> Img.ImageData.dump(); flush stderr);
  Frx_rpc.register "camltkdb" (fun _ -> Protocol.debug := not !Protocol.debug)


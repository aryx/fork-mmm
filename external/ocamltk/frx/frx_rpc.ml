(* Some notion of RPC *)
open Tk
open Protocol

(* A RPC is just a callback with a particular name, plus a Tcl procedure *)
let register name f =
  let id = new_function_id() in
  Hashtbl.add callback_naming_table id f;
  (* For rpc_info *)
  Textvariable.set (Textvariable.coerce ("camltkrpc("^name^")"))
                   (string_of_cbid id);
  tkCommand [| TkToken "proc"; TkToken name; TkToken "args";
      	    TkToken ("camlcb "^(string_of_cbid id)^" $args") |]

(* RPC *)
let invoke interp f args =
  tkEval [|
    TkToken "send";
    TkToken interp;
    TkToken f;
    TkTokenList (List.map (fun s -> TkToken s) args)
    |]

let async_invoke interp f args =
  tkCommand [|
    TkToken "send";
    TkToken "-async";
    TkToken interp;
    TkToken f;
    TkTokenList (List.map (fun s -> TkToken s) args)
    |]

let rpc_info interp =
  tkEval [|
    TkToken "send";
    TkToken interp;
    TkToken "array";
    TkToken "names";
    TkToken "camltkrpc"
    |]

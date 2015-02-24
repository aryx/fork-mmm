(*s: ./extensions/remove_simple_table.ml *)
open Safe418mmm

(*s: function Remove_simple_table.log *)
let log s = try prerr_endline s with _ -> () 
(*e: function Remove_simple_table.log *)

(* an example of html filter *)

(* This example removes the tables with atmost one <TD> (<TR> also) tag. *)
(* This reduces the widgets creations... *)
  
module Provide = struct
  let capabilities = Capabilities.get()
  end

module Mmm = Get(Provide)

open Html

(*s: enum Remove_simple_table.table_token *)
type table_token =
    ChildTable of Html.token list
  | Token of Html.token 
(*e: enum Remove_simple_table.table_token *)

(*s: enum Remove_simple_table.rst_env *)
type rst_env = {
    mutable tokens : table_token list;
    mutable trs : int;
    mutable tds : int 
  } 
(*e: enum Remove_simple_table.rst_env *)

(*s: function Remove_simple_table.remove_simple_table *)
let remove_simple_table parentf = 
  let stack = ref [] in

  let push_tbl tbl = stack := tbl :: !stack in
  let pop_tbl () =
   match !stack with
   | t :: tl -> stack := tl; t
   | _ -> assert false in
  let head_stack () = List.hd !stack in
  let empty_stack () = !stack = [] in

  let flush_childtable tkns =
    if empty_stack () then List.iter parentf tkns
    else begin
      let top = head_stack () in
      top.tokens <- top.tokens @ [ChildTable tkns]
    end
  in

  fun tkn -> match tkn with
    EOF ->
      while not (empty_stack ()) do
    log "EOFflush";
    let tbl = pop_tbl () in
    flush_childtable 
      (List.fold_right (fun xtkn st ->
        match xtkn with 
          Token tkn -> tkn :: st
        | ChildTable tkns -> tkns @ st) tbl.tokens [])
      done;
      parentf EOF
  | OpenTag {tag_name = "table"} ->
      log "ENTER";
      let tbl = {tokens= [Token tkn]; trs= 0; tds= 0} in
      push_tbl tbl;
  | CloseTag "table" when not (empty_stack ()) -> 
      let tbl = pop_tbl () in
      log "REMOVE";
      let tokens = 
    if tbl.trs <= 1 && tbl.tds <= 1 then begin
      log "ERASE";
      let tokens = 
       (* remove table, tr, td *)
       List.fold_right (fun xtkn st ->
          match xtkn with
        Token tkn -> begin
          match tkn with
            OpenTag {tag_name= "table"}
          | OpenTag {tag_name= "tr"}
          | OpenTag {tag_name= "td"}
          | CloseTag "table"
          | CloseTag "tr"
          | CloseTag "td" -> st
          | _ -> tkn :: st
        end
          |	ChildTable tkns ->
          tkns @ st) tbl.tokens []
      in
      [OpenTag {tag_name="br"; attributes=[]};
        CloseTag "br"; 
        PCData "[[" ] @ tokens @
      [ PCData "]]";
        OpenTag {tag_name="br"; attributes=[]}; 
        CloseTag "br" ]
    end else begin
      tbl.tokens <- tbl.tokens @ [Token tkn];
      List.fold_right (fun xtkn st ->
        match xtkn with
          Token tkn -> tkn :: st
        | ChildTable tkns -> tkns @ st) tbl.tokens []
    end
      in
      flush_childtable tokens
  | _ when not (empty_stack ()) -> 
      let tbl = head_stack () in
      begin
    match tkn with
      OpenTag {tag_name = "td"} ->
        tbl.tds <- tbl.tds + 1
    | OpenTag {tag_name = "tr"} -> 
        tbl.trs <- tbl.trs + 1
    | _ -> ()
      end;
      tbl.tokens <- tbl.tokens @ [Token tkn]
  | _ ->  (* !stack = [] *)
      parentf tkn
(*e: function Remove_simple_table.remove_simple_table *)

(*s: toplevel Remove_simple_table._1 *)
let _ =
  Mmm.add_html_filter remove_simple_table
(*e: toplevel Remove_simple_table._1 *)
(*e: ./extensions/remove_simple_table.ml *)

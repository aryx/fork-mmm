(*s: ./html/html_eval.ml *)
open Printf
open Html
open Dtd

(*s: type Html_eval.minimization *)
(* Wrapped up lexer to insert open/close tags in the stream of "normal"
   tokens, according to some DTD, in order to always get fully parenthesized
   streams *)

type minimization =
  Legal | Illegal of string
(*e: type Html_eval.minimization *)

(*s: constant Html_eval.debug *)
let debug = ref false
(*e: constant Html_eval.debug *)

(*s: exception Html_eval.CantMinimize *)
exception CantMinimize			            (* bogus HTML *)
(*e: exception Html_eval.CantMinimize *)

(*s: constant Html_eval.initial *)
(* initial element of the DTD *)
let initial = Elements.add "html" Elements.empty
(*e: constant Html_eval.initial *)


(*s: function Html_eval.dump_stack *)
let dump_stack () = function
    (x,_)::(y,_)::(z,_)::_ -> sprintf "..<%s><%s><%s>" z y x
  | [x,_;y,_] -> sprintf "<%s><%s>" y x
  | [x,_] -> sprintf "<%s>" x
  | [] -> "empty stack"
(*e: function Html_eval.dump_stack *)

(*s: function Html_eval.ominimize *)
(* open minimize 
   [ominimize dtd open_tag current_stack]
   returns a list of inferred open/close tags and the new stack
 *)
let ominimize dtd t stack =
  let elem = t.tag_name in

  (* Is elem allowed for the given stack ? *)
  let goodpos = function
      [] -> Elements.mem elem initial
    | (_, cts)::l -> Elements.mem elem cts

  (* Return with inferred and stack.
     The stack has been reduced during the inference, so it is enough
     to push the opened element *)
  (* Special hack when t is fake #pcdata... *)
  and return inferred stack =
    if elem = "#pcdata" then
      List.rev inferred, stack
    else
      List.rev ((OpenTag t) :: inferred),
      (elem, Hashtbl.find dtd.contents elem) :: stack
      
  in
  (* [attempt_close mods_so_far current_stack] *)
  let rec attempt_close accu = function
     [] -> (* reached all the possible closing, attempt to open again *)
        attempt_open accu []
   | ((last, _)::l) as stack ->
       if Elements.mem last dtd.close_omitted then
          (* we can attempt to close the previous element *)
      if goodpos l then 
        (* good position, we're done *)
        return ((CloseTag last) :: accu) l
          else (* attempt to open in this new position *)
        try 
              attempt_open ((CloseTag last) :: accu) l
            with
          CantMinimize -> (* try once more to close *)
             attempt_close ((CloseTag last)::accu) l
       else begin (* since we can't close, try to open *)
      attempt_open accu stack
       end

   (* [attempt_open mods_so_far currentstack] *)
   and attempt_open accu = function
     [] -> 
       (* open HTML, and retry from there *)
       (* should actually iterate on all elements in initial *)
       let newstack = ["html", Hashtbl.find dtd.contents "html"]
       and newaccu = (OpenTag {tag_name = "html"; attributes = []}) :: accu
       in
      if goodpos newstack then return newaccu newstack
          else attempt_open newaccu newstack

   | ((_, cts)::l ) as stack ->
       (* check if, in contents, there is an element with implicit omission
          that would help *)
       let possible = Elements.inter cts dtd.open_omitted in
        match Elements.cardinal possible with
      0 -> (* argh *) raise CantMinimize
        | 1 -> 
      (* open this element and try from there *)
      let newelem = Elements.choose possible in
      let newaccu = (OpenTag {tag_name = newelem; attributes = []})::accu
          and newstack = (newelem, Hashtbl.find dtd.contents newelem)::stack
          in
        if goodpos newstack 
        then return newaccu newstack
        else attempt_open newaccu newstack (* maybe more ? *)
        | n -> (* since we have the choice, examine all possibilities *)
       let elems = Elements.elements possible in
       let rec backtrack = function 
             [] -> raise CantMinimize
        | x::l -> 
        try
          let newaccu = (OpenTag {tag_name = x; attributes = []})::accu
          and newstack = (x, Hashtbl.find dtd.contents x)::stack
                  in
            if goodpos newstack then return newaccu newstack 
            else attempt_open newaccu newstack
        with
         CantMinimize -> backtrack l
           in 
       backtrack elems
  in
   (* now do some error recovery *)   
   try Legal, attempt_close [] stack
   with
     CantMinimize ->
       (* what the hell, dammit, open it anyway, who cares, duh *)
       let _currentTODO = match stack with (x,_)::l -> x | [] -> "" in
       Illegal (sprintf "illegal <%s> in %a, keep it though"
                t.tag_name dump_stack stack),
       return [] stack
(*e: function Html_eval.ominimize *)

(*s: function Html_eval.cminimize *)
(* close minimize
   [cminimize dtd elem current_stack]
   returns a list of inferred open/close tags and the new stack
 *)
let cminimize dtd tagname stack =
  (* Is elem allowed for the given stack ? *)
  let goodpos = function
      [] -> false
    | (elem, cts)::l -> tagname = elem

  and return inferred stack =
     List.rev ((CloseTag tagname) :: inferred), stack

  in
  (* [attempt_close mods_so_far current_stack] *)
  let rec attempt_close accu = function
     [] -> raise CantMinimize
   | ((last, _)::l) as _stackTODO ->
       if Elements.mem last dtd.close_omitted then
          (* we can attempt to close the previous element *)
      if goodpos l then 
        (* good position, we're done *)
        return (CloseTag last :: accu) (List.tl l)
          else (* close a bit more ? *)
        attempt_close ((CloseTag last)::accu) l
       else 
     (* there's no reason we should have to open a new element in order
        to close the current one, is it ? *)
          raise CantMinimize
  in
  (* error recovery strategy *)
  let rec attempt_matching accu = function
     [] -> raise Not_found (* didn't find a matching open at all ! *)
   | (curelem,_):: l when curelem = tagname ->
     (* so, consider we match this open, and close them all *)
     return accu l
   | (curelem,_):: l  -> (* otherwise, find something up there *)
     attempt_matching (CloseTag curelem :: accu) l
   in
   (* now do some error recovery *)   
   try Legal, attempt_close [] stack
   with
     CantMinimize ->
       try
     Illegal (sprintf "unmatched </%s> in %a, close closest match"
                  tagname dump_stack stack),
         attempt_matching [] stack 
       with
     Not_found -> 
       Illegal (sprintf "unmatched </%s> in %a, skipped"
                    tagname dump_stack stack),
           ([], stack) (* just skip the damn thing *)
(*e: function Html_eval.cminimize *)

(*s: function Html_eval.is_cdata *)
let is_cdata cts =
  Elements.cardinal cts = 1 && Elements.mem "#cdata" cts
(*e: function Html_eval.is_cdata *)

(*s: function Html_eval.sgml_lexer *)
let sgml_lexer dtd =
  let current_lex = ref Lexhtml.html in
  let stack = ref [] in
  let lexdata = Lexhtml.new_data () in

  (* currently allowed elements *)
  let allowed () = 
    match !stack with
    | [] -> initial
    | (elem, cts)::_ -> cts 
  in
  (* whatever the situation (but close), if the previous element is empty
     with an omittable close, close it *)
  let close_empty () = 
    match !stack with
    | [] -> []
    | (elem, ctx)::l ->
        if Elements.is_empty ctx && Elements.mem elem dtd.close_omitted
        then (stack := l; [CloseTag elem])
        else []
  in  
  (fun lexbuf ->
    let warnings, token, loc = !current_lex lexbuf lexdata in
    if !debug 
    then begin prerr_string "got "; Html.print token; prerr_newline() end;
    let status, tokens = 
      match token with
      | OpenTag t ->
          begin try (* first check that we know this element *)
            let contents = Hashtbl.find dtd.contents t.tag_name in
            let extraclose = close_empty() in    
            (* check changing of lexers; this works only if error recovery
               rules imply that the tag will *always* be open
             *)
            if is_cdata contents 
            then current_lex := Lexhtml.cdata
            else current_lex := Lexhtml.html;

            (* is it allowed in here ? *)
            if Elements.mem t.tag_name (allowed()) then begin
              (* push on the stack *)
              stack := (t.tag_name, contents) :: !stack;
              Legal, extraclose @ [token]
            end else begin (* minimisation or error *)
              let flag, (res, l) = ominimize dtd t !stack in
              stack := l;
              flag, extraclose @ res
            end
          with Not_found -> 
            (* Not in the DTD ! We return it, but don't change our state
               or stack. An applet extension to the HTML display machine
               can attempt to do something with it *)
            Illegal (sprintf "Element %s not in DTD" t.tag_name),
            [token]
         end
        
      | CloseTag t ->
          begin try (* do we know this element *)
            let _ = Hashtbl.find dtd.contents t in
            match !stack with
            | [] -> 
              Illegal(sprintf "Unmatched closing </%s>" t), []
            | (elem, cts)::l when elem = t -> (* matching close *)
                 stack := l; (* pop the stack *)
                 (* the lexer has to be "normal" again, because CDATA
                    can't be nested anyway *)
                 current_lex := Lexhtml.html;
                 Legal, [token]
            | (elem, cts)::l -> (* unmatched close ! *)
                (* if we were in cdata, change the token to cdata *)
                 if is_cdata cts 
                 then Legal, [CData (sprintf "</%s>" t)]
                 else begin
                   current_lex := Lexhtml.html;
                   let flag, (res, l) = cminimize dtd t !stack in
                   stack := l;
                   flag, res
                 end
        with Not_found ->
          Illegal (sprintf "Element %s not in DTD" t),
          [token]
        end
     | PCData s ->
         let extraclose = close_empty() in    
         (* is it allowed in here ? *)
         if Elements.mem "#pcdata" (allowed()) 
         then  Legal, extraclose @ [token]
          (* ignore PCData made of spaces if not relevant to the context *)
         else 
           if issp s 
           then Legal, extraclose
           else begin	    
              (* bad hack. make believe that we try to open the #pcdata element *)
             let flag, (res, l) = 
             ominimize dtd {tag_name = "#pcdata"; attributes = []} !stack in
             stack := l;
             flag,  extraclose @ res @ [token]
           end

    (* CData never happens with an empty stack *)
    | CData s ->
        let extraclose = close_empty() in    
        if Elements.mem "#cdata" (allowed()) 
        then Legal, extraclose @ [token]
        else
           Illegal(sprintf "Unexpected CDATA in %a" dump_stack !stack),
           extraclose @ [token]
        
    (* See if the stack is empty *)
    | EOF ->
       begin 
        match !stack with
        | [] -> Legal, [EOF]
        | l ->
           (* we must be able to close all remaining tags *)
           let rec closethem tokens = function
           | [] -> None, EOF :: tokens
           | (last,_) :: l ->
               if Elements.mem last dtd.close_omitted 
               then closethem (CloseTag last::tokens) l
               else
                 let status, tokens = 
                   closethem (CloseTag last::tokens) l in
                 let err = sprintf "</%s>" last in
                 let newstatus = 
                   match status with
                   | Some s -> Some (err^s)
                   | None -> Some err 
                 in
                 newstatus, tokens
         in
         match closethem [] l with
         | None, tokens -> Legal, List.rev tokens
         | Some s, tokens -> Illegal ("Missing "^s), List.rev tokens
       end

    | _ ->  Legal, [token] (* ignore all other cases *)
      
  in
  warnings, status, tokens, loc)
(*e: function Html_eval.sgml_lexer *)

(*s: constant Html_eval.filters *)
let filters = ref []
(*e: constant Html_eval.filters *)
(*s: function Html_eval.add_html_filter *)
let add_html_filter f =
  filters := f :: !filters
(*e: function Html_eval.add_html_filter *)

(*s: function Html_eval.sgml_lexer (./html/html_eval.ml) *)
(* Redefine sgml_lexer with filters *)
let sgml_lexer dtd = 
  let org_lexer = sgml_lexer dtd in
  let buf = ref [] in
  let allfilter = 
    List.fold_right (fun f st -> f st) !filters 
      (fun tkn -> buf := !buf @ [tkn]) 
  in
  function lexbuf ->
    let warnings, correct, tokens, loc = org_lexer lexbuf in
    tokens |> List.iter allfilter; (* inefficient *)
    let tokens = !buf in 
    buf := [];
    warnings, correct, tokens, loc
(*e: function Html_eval.sgml_lexer (./html/html_eval.ml) *)
  
  
(*s: function Html_eval.automat *)
let automat dtd lexbuf action error =
  try
    let lexer = sgml_lexer dtd in
    while true do
      try 
        let warnings, correct, tokens, loc = lexer lexbuf in
        warnings |> List.iter (fun (reason, pos) -> 
          error (Loc(pos,succ pos)) reason
        );
        (match correct with
        | Legal -> ()
        | Illegal reason -> error loc reason
        );
        tokens |> List.iter (function token -> 
          (try 
            (* calling the callback *)
            action loc token
          with Invalid_Html s -> error loc s
          );
          if token = EOF 
          then failwith "quit_html_eval"
        )
      with Html_Lexing (s,n) -> error (Loc(n,n+1)) s
    done
  with Failure "quit_html_eval" -> ()
(*e: function Html_eval.automat *)
(*e: ./html/html_eval.ml *)

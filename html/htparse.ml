(*s: ./html/htparse.ml *)
(* Testing the HTML Lexer/evaluator *)
open Html
open Printf

(*s: toplevel Htparse._1 *)
let _ = 
  Html.verbose := false (* we do our own error report *)
(*e: toplevel Htparse._1 *)

(*s: type Htparse.mode *)
type mode =
  Check | Indent of int | Nesting
(*e: type Htparse.mode *)

(*s: constant Htparse.verbose *)
let verbose = ref false
(*e: constant Htparse.verbose *)
(*s: constant Htparse.mode *)
let mode = ref Check
(*e: constant Htparse.mode *)

(*s: function Htparse.error *)
let error name find_line (Loc(n,n')) msg =
  let linenum, linestart = find_line n in
  printf "File \"%s\", line %d, characters %d-%d:\n%s\n"
         name linenum (n - linestart) (n' - linestart) msg
(*e: function Htparse.error *)


(*s: function Htparse.line_reporting *)
(* lines: start at 1 *)
(* pos: start at 0 as in caml *)
let line_reporting ic =
  let lines = ref [] 
  and current_line = ref 1
  and current_pos = ref 0 in
  let read = 
    (*
    if !Lang.japan 
    then 
      (Japan.create_read_japanese (input ic) (Japan.default_config ()))#read
    else 
    *)
    input ic 
  in
  Lexing.from_function (fun buf len ->
     let n = read buf 0 len in
       for i = 0 to n - 1 do
     match buf.[i] with
       '\n' -> incr current_pos; incr current_line;
               lines := (!current_pos, !current_line) :: !lines
         | _ -> incr current_pos
         done; 
         n),
  (fun pos ->
    let rec find_line = function
      [] -> 1, 0
    | (linestart, linenum)::l when pos < linestart -> find_line l
    | (linestart, linenum)::l -> linenum, linestart
    in
     find_line !lines)
(*e: function Htparse.line_reporting *)

(*s: function Htparse.html_lex *)
let html_lex name =
  let ic = open_in name in
  let lexbuf, find_line = line_reporting ic in
  Html_eval.automat Dtd.dtd32f
     (fun loc token ->
        match token with
        | EOF -> close_in ic
        | t -> if !verbose then (Html.print t; flush stdout)
     )
     lexbuf
     (error name find_line)
(*e: function Htparse.html_lex *)

(*s: function Htparse.html_nest *)
let html_nest name =
  let ic = open_in name in
  let lexbuf = Lexing.from_channel ic in
  let stack = ref [] in
   Html_eval.automat Dtd.dtd32f
      (function Loc(n,n') ->
      function 
        EOF -> close_in ic
          | OpenTag t ->
          stack := t.tag_name :: !stack
          | CloseTag t ->
          begin match !stack with
            hd::tl when hd = t -> stack := tl
              | hd::tl -> eprintf "Unmatched closing tag %s (expected %s) at 
                            pos %d - %d" t hd n n'
          | [] -> eprintf "Unmatched closing tag %s (Empty stack) at
                            pos %d - %d" t n n'
              end
          | _ -> ())
      lexbuf
      (fun _ _ -> ())
(*e: function Htparse.html_nest *)

(*s: function Htparse.html_indent *)
let html_indent name level =
  let box = match level with
     0 -> Format.open_box
   | 1 -> Format.open_hvbox
   | n -> Format.open_vbox in
  let ic = open_in name in
  let lexbuf  = Lexing.from_channel ic in
   box 0;
   Html_eval.automat Dtd.dtd32f
      (fun loc token ->
      match token with
        EOF -> 
        Format.print_newline();
        close_in ic
          | OpenTag t ->
                Format.print_cut();
        box 0;
        box 2;
        Format.print_string (sprintf "<%s>" t.tag_name)
      | CloseTag t ->
        Format.close_box();
                Format.print_cut();
        Format.print_string (sprintf "</%s>" t);
                Format.close_box()
      | PCData _ -> Format.print_string "*"
      | _ -> ()
      )
      lexbuf
      (fun _ msg -> Format.print_string (sprintf "ERROR(%s)" msg))
(*e: function Htparse.html_indent *)

(*s: function Htparse.main *)
let main () =
  Html.init (Lang.lang());

  Arg.parse [
     "-debug", Arg.Unit (function () -> Html_eval.debug := true), "Debug mode";
     "-strict", Arg.Set Lexhtml.strict, "Strict mode";
     "-v", Arg.Unit (function () -> verbose := true), "Verbose mode";
     "-struct", Arg.Int (function n -> mode := Indent n), "Parse Tree";
     "-nesting", Arg.Unit (function () -> mode := Nesting), "Check nesting";
     "-dtd", Arg.Unit (function () -> Dtd.dump Dtd.dtd32f), "Dump DTD";
     "-depth", Arg.Int (function n -> Format.set_max_boxes n), "Max print depth"
     ]
     (fun s -> 
       match !mode with
       | Check -> html_lex s
       | Indent n -> html_indent s n
       | Nesting -> html_nest s
       )
     "Usage: htparse <opts> file1.html ... filen.html"
(*e: function Htparse.main *)

(*s: toplevel Htparse._2 *)
let _ = Printexc.catch main ()
(*e: toplevel Htparse._2 *)
(*e: ./html/htparse.ml *)

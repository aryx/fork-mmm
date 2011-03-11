open Tables
open Compile
open Intf

let input_name = ref "Widgets.src"
let output_dir = ref ""
let destfile f = Filename.concat !output_dir f

let prerr_error_header ic =
  close_in ic;
  prerr_string "File \""; prerr_string !input_name;
  prerr_string "\", line ";
  prerr_string (string_of_int !Lexer.current_line);
  prerr_string ": "

let parse_file filename =
  let ic = open_in_bin filename in
  try
    let lexbuf = Lexing.from_channel ic in
      while true do
       Parser.entry Lexer.main lexbuf
      done
  with
  | Parsing.Parse_error ->
      prerr_error_header ic;
      prerr_string "Syntax error \n";
      exit 1
  | Lexer.Lexical_error s ->
      prerr_error_header ic;
      prerr_string "Lexical error (";
      prerr_string s;
      prerr_string ")\n";
      exit 1
  | Duplicate_Definition (s,s') ->
      prerr_error_header ic;
      prerr_string s; prerr_string " "; prerr_string s';
      prerr_string " is defined twice.\n";
      exit 1
  | Compiler_Error s ->
      prerr_error_header ic;
      prerr_string "Internal error: "; prerr_string s; prerr_string "\n";
      prerr_string "Please report bug\n";
      exit 1
  | End_of_file ->
      close_in ic

(* hack to provoke production of cCAMLtoTKoptions_constrs *)
let option_hack oc =
  try
   let typdef = Hashtbl.find types_table "options" in
   let hack =
     {parser_arity = OneToken;
      constructors = 
        List.map
          (fun c -> 
             { component = Constructor;
               ml_name = "C" ^ c.ml_name;
               template =
                 begin match c.template with
                 | ListArg (x :: _) -> x
                 | _ -> fatal_error "bogus hack"
                 end;
               result = UserDefined "options_constrs";
               safe = true}) 
        typdef.constructors;
      subtypes = [];
      requires_widget_context = false} in
   write_CAMLtoTK (output_string oc) "options_constrs" hack
  with Not_found -> ()

let compile () = 
  let oc = open_out_bin (destfile "tkgen.ml") in
  let sorted_types = Tsort.sort types_order in
  List.iter
    (fun typname ->
       try
         let typdef = Hashtbl.find types_table typname in
         write_type (output_string oc) typname typdef;
         write_CAMLtoTK (output_string oc) typname typdef;
         if List.mem typname !types_returned then
            write_TKtoCAML (output_string oc) typname typdef
       with 
       | Not_found -> 
           if not (List.mem_assoc typname !types_external) then begin
             prerr_string "Type ";
             prerr_string typname;
             prerr_string " is undeclared external or undefined\n";
             exit 1
           end)
    sorted_types;
  option_hack oc;
  List.iter (write_function (output_string oc)) !function_table;
  close_out oc;
  (* Write the interface for public functions *)
  (* this interface is used only for documentation *)
  let oc = open_out_bin (destfile "tkgen.mli") in
  List.iter
    (write_function_type (output_string oc))
    (sort_components !function_table);
  close_out oc;
  let write_module wname wdef =
    let modname = String.uncapitalize wname in
    let oc = open_out_bin (destfile (modname ^ ".ml"))
    and oc' = open_out_bin (destfile (modname ^ ".mli")) in
    begin match wdef.module_type with
    | Widget -> output_string oc' ("(* The " ^ wname ^ " widget *)\n")
    | Family -> output_string oc' ("(* The " ^ wname ^ " commands  *)\n")
    end;
    let open_std oc =
      output_string oc "open Tk\n";
      output_string oc "open Widget\n";
      output_string oc "open Textvariable\n" in
    output_string oc "open Protocol\n";
    open_std oc;
    open_std oc';
    begin match wdef.module_type with
    | Widget ->
        write_create (output_string oc) wname;
        write_named_create (output_string oc) wname;
        write_create_p (output_string oc') wname;
        write_named_create_p (output_string oc') wname;
    | Family -> ()
    end;
    List.iter
      (write_function (output_string oc))
      (sort_components wdef.commands);
    List.iter
      (write_function_type (output_string oc'))
      (sort_components wdef.commands);
    List.iter
      (write_external (output_string oc))
      (sort_components wdef.externals);
    List.iter
      (write_external_type (output_string oc'))
      (sort_components wdef.externals);
    close_out oc;
    close_out oc' in
  Hashtbl.iter write_module module_table;
  (* write the module list in file ``modules'' for the Makefile *)
  let oc = open_out_bin (destfile "modules") in
  output_string oc "WIDGETOBJS=";
  Hashtbl.iter
    (fun name _ ->
       output_string oc (String.uncapitalize name);
       output_string oc ".cmo ")
    module_table;
  output_string oc "\n";
  close_out oc

let main () =
  Arg.parse
    ["-outdir", Arg.String (fun s -> output_dir := s), "Output directory";
     "-I",
     Arg.String
       (fun s -> Compile.search_path := !Compile.search_path @ [s]),
     "Search path for external files"]
    (fun filename -> input_name := filename) 
    "Usage: tkcompiler <source file> -outdir <output directory>";
  if !output_dir = "" then begin
    prerr_endline "specify -outdir option"; exit 1
  end;
  try 
    parse_file !input_name;
    compile ();
    exit 0
  with
  | Lexer.Lexical_error s ->
      prerr_string "Invalid lexical character: ";
      prerr_endline s;
      exit 1
  | Duplicate_Definition (s, s') ->
      prerr_string s; prerr_string " "; prerr_string s';
      prerr_endline " is illegally redefined";
      exit 1
  | Invalid_implicit_constructor c ->
      prerr_string "Constructor ";
      prerr_string c;
      prerr_endline " is implicitly used before being defined";
      exit 1
  | Tsort.Cyclic ->
      prerr_endline "Cyclic dependency of types";
      exit 1
  | Compiler_Error s ->
      prerr_endline ("Compiler error: " ^ s);
      exit 1;;

Printexc.catch main ();;

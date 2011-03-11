open Tables

(*
 * Pretty print a type
 *  used to write ML type definitions
 *)
let rec ppMLtype =
  function
  | Unit -> "unit"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | List ty -> ppMLtype ty ^ " list"
  | Product tyl -> String.concat " * " (List.map ppMLtype tyl)
  | UserDefined s -> s
  | Subtype (s, _) -> s
  | Function (Product tyl) ->
      "(" ^ (String.concat " -> " (List.map ppMLtype tyl)) ^ " -> unit)"
  | Function ty ->
      "(" ^ ppMLtype ty ^ " -> unit)"

(* Extract all types from a template *)
let rec types_of_template = function
  | StringArg _ -> []
  | TypeArg t -> [t]
  | ListArg l -> List.flatten (List.map types_of_template l)

(* Produce a documentation version of a template *)
let rec ppTemplate = function
  | StringArg s -> s
  | TypeArg t -> "<" ^ ppMLtype t ^ ">"
  | ListArg l -> "{" ^ String.concat " " (List.map ppTemplate l) ^ "}"

let doc_of_template = function
  | ListArg l -> String.concat " " (List.map ppTemplate l)
  | t -> ppTemplate t

(*
 * Type definitions
 *)
(* Write an ML constructor *)
let write_constructor w {ml_name = mlconstr; template = t} =
  w mlconstr;
  begin match types_of_template t with
  | [] -> ()
  | l -> w " of "; w (ppMLtype (Product l))
  end;
  w "    (* tk option: "; w (doc_of_template t); w " *)"

(* Write a rhs type decl *)
let write_constructors w =
  List.iter (fun c -> w "\n   | "; write_constructor w c)

(* List of constructors, for runtime subtyping *)
let write_constructor_set w sep =
  List.iter (fun c -> w sep; w ("C" ^ c.ml_name))

let write_constructor_list w sep =
  List.iter (fun c -> w ("C" ^ c.ml_name); w sep)


(* Definition of a type *)
let write_type w name typdef =
  (* The type itself *)
  (* Put markers for extraction *)
  w "(* type *)\n";
  w ("type " ^ name ^ " =");
  write_constructors w (sort_components typdef.constructors);
  w "\n(* /type *)\n\n";
  (* Dynamic Subtyping *)
  if typdef.subtypes <> [] then begin
    (* The set of its constructors *)
    (* sp before "type" to avoid being picked up in documentation *)
    if name = "options" then w "(* type *)\n";
    w ("type " ^ name ^ "_constrs =");
    write_constructor_set w "\n   | " (sort_components typdef.constructors);
    if name = "options" then w "\n(* /type *)";
    w "\n\n\n";
    (* The set of all constructors *)
    w ("let " ^ name ^ "_any_table = [");
    write_constructor_list w "; " (sort_components typdef.constructors);
    w ("]\n\n");
    (* The subset of constructors for each subtype *)
    List.iter
      (function (s, l) ->
         w ("let " ^ name ^ "_" ^ s ^ "_table = [\n");
         write_constructor_list w "; " (sort_components l);
         w ("\n]\n\n"))
      typdef.subtypes
  end

(************************************************************)
(* Converters                                               *)
(************************************************************)

let rec converterTKtoCAML argname = function
 | Int -> "int_of_string " ^ argname
 | Float -> "float_of_string " ^ argname
 | Bool ->
     "(match " ^ argname ^ " with
       | \"1\" -> true
       | \"0\" -> false
       | s -> raise (Invalid_argument (\"cTKtoCAMLbool\" ^ s)))"
 | Char -> "String.get " ^ argname ^ " 0"
 | String -> argname
 | UserDefined s -> "cTKtoCAML" ^ s ^ " " ^ argname
 | Subtype (s,s') -> "cTKtoCAML" ^ s ^ " " ^ argname
 | List ty ->
    begin match type_parser_arity ty with
    | OneToken ->
        "(List.map (function x -> " ^
        (converterTKtoCAML "x) " ty) ^ argname ^ ")"
    | MultipleToken ->
       "iterate_converter (function x -> " ^
        (converterTKtoCAML "x) " ty) ^ argname ^ ")"
    end
 | _ -> fatal_error "converterTKtoCAML"


(*******************************)
(* Wrappers                    *)
(*******************************)
let varnames prefx n =
  let rec var i =
    if i > n then [] else (prefx ^ string_of_int i) :: var (succ i) in
  var 1

(*
 * generate wrapper source for callbacks
 *  transform a function ... -> unit in a function : unit -> unit
 *  using primitives arg_ ... from the protocol
 *  Warning: sequentiality is important in generated code
 *  TODO: remove arg_ stuff and process lists directly ?
 *)

let wrapper_code fname = function
  | Unit -> "(function _ -> " ^ fname ^ " ())"
  | ty ->
      "(function args ->\n    " ^
      begin match ty with
      | Product tyl ->
         (* variables for each component of the product *)
         let vnames = varnames "a" (List.length tyl) in
         (* getting the arguments *)
         let readarg =
           List.map2
             (fun v ty ->
                match type_parser_arity ty with
                | OneToken ->
                    "let (" ^ v ^ ", args) = " ^
                    converterTKtoCAML "(List.hd args)" ty ^
                    ", List.tl args in\n    "
                | MultipleToken ->
                    "let (" ^ v ^ ", args) = " ^
                    converterTKtoCAML "args" ty ^
                    "in\n    ")
             vnames tyl in
         String.concat "" readarg ^ fname ^ " " ^ String.concat " " vnames
      (* all other types are read in one operation *)
      | List elty ->
          fname ^ " (" ^ converterTKtoCAML "args" ty ^ ")"
      | String ->
          fname ^ " (" ^ converterTKtoCAML "(List.hd args)" ty ^ ")"
      | ty ->
          begin match type_parser_arity ty with
          | OneToken ->
              fname ^ " (" ^ converterTKtoCAML "(List.hd args)" ty ^ ")"
          | MultipleToken ->
              "let (v, _) = " ^
              converterTKtoCAML "args" ty ^
              " in\n    " ^ fname ^ " v"
          end
      end ^ ")"

(*************************************************************)
(* Parsers                                                   *)
(*  are required only for values returned by commands and    *)
(*  functions (table is computed by the parser)              *)

(* Tuples/Lists are Ok if they don't contain strings         *)
(* they will be returned as list of strings                  *)

(* Can we generate a "parser" ?
   -> all constructors are unit and at most one int and one string,
   with null constr
*)
type parser_pieces = {
   mutable zeroary : (string * string) list ; (* kw string, ml name *)
   mutable intpar : string list; (* one at most, mlname *)
   mutable stringpar : string list (* idem *)
}

type mini_parser =
   | NoParser
   | ParserPieces of parser_pieces

let can_parse pp constructors =
  List.for_all
    (function c ->
       match c.template with
       | ListArg [StringArg s] ->
           pp.zeroary <- (s, c.ml_name) :: pp.zeroary;
           true
       | ListArg [TypeArg Int]
       | ListArg [TypeArg Float] when pp.intpar = [] ->
           pp.intpar <- [c.ml_name];
           true
       | ListArg [TypeArg String] when pp.stringpar = [] ->
           pp.stringpar <- [c.ml_name];
           true
       | _ -> false)
    constructors

let can_generate_parser constructors =
  let pp = {zeroary = []; intpar = []; stringpar = []} in
  if can_parse pp constructors then ParserPieces pp else NoParser

(* We can generate parsers only for simple types *)
(* we should avoid multiple walks *)
let write_TKtoCAML w name typdef =
  if typdef.parser_arity = MultipleToken then
    prerr_endline
      ("You must write cTKtoCAML" ^ name ^
       " : string list -> " ^ name ^ " * string list")
  else match can_generate_parser typdef.constructors with
  | NoParser ->
      prerr_endline
        ("You must write cTKtoCAML" ^ name ^ " : string ->" ^ name)
  | ParserPieces pp -> begin
      w ("let cTKtoCAML" ^ name ^ " n =\n");
      (* First check integer *)
      if pp.intpar <> [] then begin
        w ("   try " ^ List.hd pp.intpar ^ " (int_of_string n)\n");
        w ("   with _ ->\n") end;
      w ("   match n with\n");
      List.iter
        (fun (tk, ml) -> w "   | \""; w tk; w "\" ->\n     "; w ml; w "\n")
          pp.zeroary;
      let final =
        if pp.stringpar <> []
        then "   | n -> " ^ List.hd pp.stringpar ^ " n"
        else "   | s -> raise (Invalid_argument (\"cTKtoCAML" ^
             name ^ ": \" ^ s))" in
      w final;
      w "\n\n"
     end

(******************************)
(* Converters                 *)
(******************************)

let add_context_widget w s name args =
  let args =
   if requires_widget_context s then w ^ " " ^ args else args in
  name ^ args

(* Produce an in-lined converter Caml -> Tk for simple types *)
(* the converter is a function of type:  <type> -> string  *)
let rec converterCAMLtoTK context_widget argname = function
  | Int -> "TkToken (string_of_int " ^ argname ^ ")"
  | Float -> "TkToken (string_of_float " ^ argname ^ ")"
  | Bool -> "if " ^ argname ^ " then TkToken \"1\" else TkToken \"0\""
  | Char -> "TkToken (Char.escaped " ^ argname ^ ")"
  | String -> "TkToken " ^ argname
  | UserDefined s ->
      let name = "cCAMLtoTK" ^ s ^ " " in
      let args =
        if is_subtyped s
        then  (* unconstraint subtype *)
          s ^ "_any_table " ^ argname
        else argname in
      add_context_widget context_widget s name args
  | Subtype (s, s') ->
      let name = "cCAMLtoTK" ^ s ^ " " in
      let args = s ^ "_" ^ s' ^ "_table " ^ argname in
      add_context_widget context_widget s name args
  | Function _ -> fatal_error "unexpected function type in converterCAMLtoTK"
  | Unit -> fatal_error "unexpected unit type in converterCAMLtoTK"
  | Product _ -> fatal_error "unexpected product type in converterCAMLtoTK"
  | List _ -> fatal_error "unexpected list type in converterCAMLtoTK"

(*
 * Produce a list of arguments from a template
 *  The idea here is to avoid allocation as much as possible
 *
 *)

let code_of_template funtemplate context_widget template =
  let variables = ref []
  and varcnter = ref 0 in
  let newvar () =
    incr varcnter;
    let v = "v" ^ string_of_int !varcnter in
    variables := v :: !variables;
    v in
  let rec coderec = function
    | StringArg s -> "TkToken \"" ^ s ^ "\""
    | TypeArg (List ty) ->
        "TkTokenList (\n        List.map (function x -> " ^
        converterCAMLtoTK context_widget "x" ty ^
        " )\n           " ^ newvar () ^ "\n        )"
    | TypeArg (Function tyarg) ->
        "let id = register_callback " ^ context_widget ^
        " " ^ wrapper_code (newvar ()) tyarg ^
        " in\n         TkToken (\"camlcb \" ^ id)"
   | TypeArg ty -> converterCAMLtoTK context_widget (newvar ()) ty
   | ListArg l ->
        "TkQuote (TkTokenList [\n        " ^
        String.concat ";\n        " (List.map coderec l) ^
        "\n        ])" in

  let code =
    if funtemplate
    then
      match template with
      | ListArg l ->
          "[|\n        " ^
          String.concat ";\n        " (List.map coderec l) ^
          "\n        |]"
      | _ -> "[|\n        " ^ coderec template ^ "\n        |]"
    else
      match template with
      | ListArg [x] -> coderec x
      | ListArg l ->
          "TkTokenList [\n        " ^
          String.concat ";\n        " (List.map coderec l) ^
          "\n        ]"
      | _ -> coderec template in

  code, List.rev !variables

(*
 * Converters for user defined types
 *)

(* For each case of a concrete type *)
let write_clause w context_widget subtyp comp =
  let warrow () =
    w " ->\n      ";
    if subtyp then
    w ("chk_sub \"" ^ comp.ml_name ^ "\" table C" ^
        comp.ml_name ^ ";\n      ") in
  w comp.ml_name;
  let (code, variables) = code_of_template false context_widget comp.template in
  begin match variables with
  | [] -> warrow ()
  | [x] -> w " "; w x; warrow ()
  | l -> w " (";  w (String.concat ", " l); w ")"; warrow ()
  end;
  w code

(* The full converter *)
let write_CAMLtoTK w name typdef =
  w ("let cCAMLtoTK" ^ name);
  let context_widget =
    if typdef.requires_widget_context then begin w " w"; "w" end
    else "Widget.dummy" in
  let subtyp = typdef.subtypes <> [] in
  if subtyp then w " table";
  w " = function";
  List.iter (fun c -> w "\n  | "; write_clause w context_widget subtyp c)
            typdef.constructors;
  w "\n\n\n"

(* Tcl does not really return "lists". It returns sp separated tokens *)
let write_result_parsing w = function
  | List String ->
      w "(splitlist res)"
  | List ty ->
      w ("  List.map " ^ converterTKtoCAML "(splitlist res)" ty)
  | Product tyl ->
      let rnames = varnames "r" (List.length tyl) in
      w  "  let l = splitlist res in\n";
      w ("    if List.length l <> " ^ string_of_int (List.length tyl) ^ "\n");
      w ("    then raise (TkError (\"unexpected result: \" ^ res))\n");
      w ("    else\n");
      List.iter2
       (fun r ty ->
          w ("    let " ^ r ^ ", l =\n      ");
          begin match type_parser_arity ty with
          | OneToken ->
              w (converterTKtoCAML "(List.hd l)" ty); w (", List.tl l")
          | MultipleToken ->
              w (converterTKtoCAML "l" ty)
          end;
          w (" in\n"))
       rnames
       tyl;
       w (String.concat "," rnames)
  | String ->
      w (converterTKtoCAML "res" String)
  | ty ->
      begin match type_parser_arity ty with
      | OneToken -> w (converterTKtoCAML "res" ty)
      | MultipleToken -> w (converterTKtoCAML "(splitlist res)" ty)
      end

let write_function w def =
  w ("let " ^ def.ml_name ^ " ");
  (* a bit approximative *)
  let context_widget =
    match def.template with
    | ListArg (TypeArg (UserDefined "widget") :: _)
    | ListArg (TypeArg (Subtype ("widget", _)) :: _) -> "v1"
    | _ -> "Widget.dummy" in

  let (code, variables) = code_of_template true context_widget def.template in
  (* Arguments *)
  begin match variables with
  | [] -> w "() =\n  "
  | l -> w (String.concat " " l); w " =\n  "
  end;
  begin match def.result with
  | Unit -> w "tkCommand "; w code
  | ty -> w "let res = tkEval "; w code; w " in \n  "; write_result_parsing w ty
  end;
  w "\n\n"

let write_create w clas =
  w ("let create parent options =\n");
  w ("  let w = new_atom \"" ^ clas ^ "\" parent in\n");
  w  "  tkCommand [|\n";
  w ("    TkToken \"" ^ clas ^ "\";\n");
  w ("    TkToken (Widget.name w);\n");
  w ("    TkTokenList (List.map (function x -> " ^
     converterCAMLtoTK "w" "x" (Subtype("options", clas)) ^ ") options)\n");
  w ("            |];\n");
  w ("  w\n\n")

let write_named_create w clas =
  w ("let create_named parent name options =\n");
  w ("  let w = new_named \"" ^ clas ^ "\" parent name in\n");
  w  "  tkCommand [|\n";
  w ("    TkToken \"" ^ clas ^ "\";\n");
  w ("    TkToken (Widget.name w);\n");
  w ("    TkTokenList (List.map (function x -> " ^
     converterCAMLtoTK "w" "x" (Subtype("options", clas)) ^ ") options)\n");
  w ("            |];\n");
  w ("      w\n\n")


let search_path = ref ["."]

(* taken from utils/misc.ml *)
let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
    | [] -> raise Not_found
    | dir :: rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem in
    try_dir path
  end

(* builtin-code: the file (without suffix) is in .template... *)
(* not efficient, but hell *)
let write_external w def =
  match def.template with
  | StringArg fname ->
      begin
        try
          let realname = find_in_path !search_path (fname ^ ".ml") in
          let ic = open_in_bin realname in
          try
            while true do
              w (input_line ic);
              w "\n"
            done
          with
          | End_of_file -> close_in ic
        with
        | Not_found ->
            raise (Compiler_Error ("can't find external file: " ^ fname))
      end
  | _ -> raise (Compiler_Error "invalid external definition")

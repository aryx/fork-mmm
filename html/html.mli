(*s: ./html/html.mli *)
(*s: type Html.attribute_name *)
(* HTML tokens *)
type attribute_name = string 
(*e: type Html.attribute_name *)
(*s: type Html.attribute_value *)
type attribute_value = string
(*e: type Html.attribute_value *)
(*s: type Html.attributes *)
type attributes = (attribute_name * attribute_value) list
(*e: type Html.attributes *)

(*s: type Html.tag *)
type tag = {
  tag_name : string;
  attributes: attributes
}
(*e: type Html.tag *)

(*s: type Html.token *)
type token =
 | OpenTag of tag
 | CloseTag of string
 | PCData of string
 | CData of string
 | Comment of string
 | Doctype of string
 | EOF
(*e: type Html.token *)

(*s: type Html.location *)
type location = Loc of int * int
(*e: type Html.location *)

(*s: exception Html.Html_Lexing *)
exception Html_Lexing of string * int
(*e: exception Html.Html_Lexing *)
(*s: exception Html.Invalid_Html *)
exception Invalid_Html of string
(*e: exception Html.Invalid_Html *)


(*s: signature Html.init *)
val init : string -> unit 
(*e: signature Html.init *)

(*s: signature Html.verbose *)
val verbose : bool ref
  (* verbose mode for HTML related stuff *)
(*e: signature Html.verbose *)

(*s: signature Html.warning *)
val warning : string -> location -> unit
(*e: signature Html.warning *)

(*s: signature Html.print *)
val print : token -> unit
  (* for debugging, prints an HTML token *)
(*e: signature Html.print *)

(*s: signature Html.beautify *)
val beautify: bool -> string -> string
  (* [beautify remove_leading_space s] removes sequences of SP *)
(*e: signature Html.beautify *)

(*s: signature Html.beautify2 *)
val beautify2 : string -> string
  (* [beautify2 s] removes leading/trailing space and sequences of SP *)
(*e: signature Html.beautify2 *)

(*s: signature Html.issp *)
val issp : string -> bool
  (* [issp s] is true if s is formed only of SP *)
(*e: signature Html.issp *)

(*s: signature Html.get_entity *)
val get_entity : string -> string
  (* [get_entity "amp"] returns "&" *)
(*e: signature Html.get_entity *)

(*s: signature Html.get_attribute *)
val get_attribute : tag -> string -> string
  (* [get_attribute tag attrib_name] *)
(*e: signature Html.get_attribute *)

(*s: signature Html.has_attribute *)
val has_attribute : tag -> string -> bool
  (* [has_attribute tag attrib_name] *)
(*e: signature Html.has_attribute *)

(*s: type Html.length *)
(* HTML length *)
type length = 
    Nolength
  | LengthPixels of int
  | LengthRatio of float
  | LengthRel of int
(*e: type Html.length *)
      
(*s: signature Html.length_of_string *)
val length_of_string : string -> length
(*e: signature Html.length_of_string *)

(*e: ./html/html.mli *)

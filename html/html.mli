(* HTML tokens *)
type attribute_name = string 
type attribute_value = string
type attributes = (attribute_name * attribute_value) list

type tag = {
  tag_name : string;
  attributes: attributes
}

type token =
   PCData of string
 | CData of string
 | OpenTag of tag
 | CloseTag of string
 | Comment of string
 | Doctype of string
 | EOF

type location = Loc of int * int

exception Html_Lexing of string * int
exception Invalid_Html of string


val init : bool -> unit 

val verbose : bool ref
  (* verbose mode for HTML related stuff *)

val warning : string -> location -> unit

val print : token -> unit
  (* for debugging, prints an HTML token *)

val beautify: bool -> string -> string
  (* [beautify remove_leading_space s] removes sequences of SP *)

val beautify2 : string -> string
  (* [beautify2 s] removes leading/trailing space and sequences of SP *)

val issp : string -> bool
  (* [issp s] is true if s is formed only of SP *)

val get_entity : string -> string
  (* [get_entity "amp"] returns "&" *)

val get_attribute : tag -> string -> string
  (* [get_attribute tag attrib_name] *)

val has_attribute : tag -> string -> bool
  (* [has_attribute tag attrib_name] *)

(* HTML length *)
type length = 
    Nolength
  | LengthPixels of int
  | LengthRatio of float
  | LengthRel of int
      
val length_of_string : string -> length


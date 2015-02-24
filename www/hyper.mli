(*s: ./www/hyper.mli *)
(* An hypertext(media) link on the Web *)

(* This is currently for HTTP and derived, but ... *)
(*s: enum Hyper.link_method *)
(* Contains only the one we support *)
type link_method =
   GET 
 | HEAD
 | POST of string
(*e: enum Hyper.link_method *)

(*s: signature Hyper.parse_method *)
val parse_method : string -> link_method
(*e: signature Hyper.parse_method *)

(*s: enum Hyper.link *)
type link = {
  h_uri : string;
  h_context: string option;
  h_method : link_method;		(* default is GET *)
  h_params : (string * string) list
  }
(*e: enum Hyper.link *)

(*s: enum Hyper.link_error *)
type link_error =
    LinkResolve of string
  | UrlLexing of string * int
(*e: enum Hyper.link_error *)

(*s: exception Hyper.Invalid_link *)
exception Invalid_link of link_error
(*e: exception Hyper.Invalid_link *)

(*s: signature Hyper.urlconcat *)
val urlconcat: Url.t -> string -> string
   (* [urlconcat url relurl] resolves the relative URL [relurl] in the
       context of the URL [url]
      Doesn't handle fragments
    *)
(*e: signature Hyper.urlconcat *)

(*s: signature Hyper.resolve *)
val resolve : link -> Uri.abs_uri
  (* raises Invalid_link(msg) *)
(*e: signature Hyper.resolve *)
(*s: signature Hyper.string_of *)
val string_of : link -> string
  (* make an absolute URI (including fragment) from link 
     raises Invalid_link(msg) *)
(*e: signature Hyper.string_of *)
  

(*e: ./www/hyper.mli *)

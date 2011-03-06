(* An hypertext(media) link on the Web *)

(* This is currently for HTTP and derived, but ... *)
(* Contains only the one we support *)
type link_method =
   GET 
 | HEAD
 | POST of string

val parse_method : string -> link_method

type link = {
  h_uri : string;
  h_context: string option;
  h_method : link_method;		(* default is GET *)
  h_params : (string * string) list
  }

type link_error =
    LinkResolve of string
  | UrlLexing of string * int

exception Invalid_link of link_error

val urlconcat: Url.t -> string -> string
   (* [urlconcat url relurl] resolves the relative URL [relurl] in the
       context of the URL [url]
      Doesn't handle fragments
    *)

val resolve : link -> Uri.abs_uri
  (* raises Invalid_link(msg) *)
val string_of : link -> string
  (* make an absolute URI (including fragment) from link 
     raises Invalid_link(msg) *)
  


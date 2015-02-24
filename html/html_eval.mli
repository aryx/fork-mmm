(*s: ./html/html_eval.mli *)
(*s: signature Html_eval.debug *)
(* HTML Evaluation *)

val debug : bool ref
(*e: signature Html_eval.debug *)

(*s: enum Html_eval.minimization *)
type minimization =
  Legal | Illegal of string
(*e: enum Html_eval.minimization *)

(*s: signature Html_eval.add_html_filter *)
(* test suit *)
val add_html_filter : ((Html.token -> unit) -> Html.token -> unit) -> unit
(* [add_html_filter filter] adds an HTML filter between the lexing and
  displaying of HTML. So, the filters do not affect the source (and
  the source display), change the content of HTML silently, and affect
  the display. The filter function [filter pfilter] receives a HTML token
  for each time, and do some job, and send a token to the parent filter 
  pfilter if possible. The filters will receive a correct HTML token
  stream (all the tags are placed and closed correctly due to the DTD),
  and they must send the correct stream to the parent filter also. 
*)
(*e: signature Html_eval.add_html_filter *)
(*s: signature Html_eval.sgml_lexer *)
(* [add_html_filter filter] adds an HTML filter between the lexing and
  displaying of HTML. So, the filters do not affect the source (and
  the source display), change the content of HTML silently, and affect
  the display. The filter function [filter pfilter] receives a HTML token
  for each time, and do some job, and send a token to the parent filter 
  pfilter if possible. The filters will receive a correct HTML token
  stream (all the tags are placed and closed correctly due to the DTD),
  and they must send the correct stream to the parent filter also. 
*)

val sgml_lexer :
  Dtd.t -> Lexing.lexbuf -> 
    ((string * int) list * minimization * Html.token list * Html.location)
(*e: signature Html_eval.sgml_lexer *)

(*s: signature Html_eval.automat *)
val automat : Dtd.t -> (Html.location -> Html.token -> unit)
                    -> Lexing.lexbuf
                    -> (Html.location -> string -> unit)
            -> unit
(*e: signature Html_eval.automat *)
(*e: ./html/html_eval.mli *)

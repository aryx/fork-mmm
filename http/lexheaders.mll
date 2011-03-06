{
open Http_headers
open Www
open Lexing
(* 
    CHAR = ['\000'-'\126']
    CTL  = ['\000'-'\031' '\127']
    CHAR except CTL = ['\032'-'\126']
    tspecials = ['(' ')' '<' '>' '@' ',' ';' ':' '\\' '"' '/' '[' ']' '?' '=' 
      	       	 ' ' '\t']
*)

}

rule challenge = parse
 | [^ ' ' '\t' '\r' '\n']+
    { let scheme_name = String.lowercase (Lexing.lexeme lexbuf) in
      let scheme = 
	match scheme_name with
	  "basic" -> AuthBasic
	| _ -> AuthExtend scheme_name in
      let _ = lws lexbuf in
      let _ = starlws lexbuf in
      let realm = realm lexbuf in
      let params = authparam lexbuf in
      	{ challenge_scheme = scheme;
	  challenge_realm = realm;
	  challenge_params = params}
    }

 | _ { raise (Invalid_HTTP_header "auth-scheme expected")}

and quotedstring = parse
   '"' [^ '"' '\000'-'\031' '\127'-'\255' ]* '"'
     { let t = Lexing.lexeme lexbuf in
         String.sub t 1 (String.length t - 2)
     }
 
 | _ { raise (Invalid_HTTP_header "quotedstring expected") }


and token = parse
   [^ '\127'-'\255' 
      '\000'-'\031'
      '(' ')' '<' '>' '@' ',' ';' ':' '\\' '"' '/' '[' ']' '?' '=' ' ' '\t']+
      { Lexing.lexeme lexbuf }
  | _ { raise (Invalid_HTTP_header "token expected") }

(* value = token | quoted-string *)
and value = parse
   '"' [^ '"' '\000'-'\031' '\127'-'\255' ]* '"'
     { let t = Lexing.lexeme lexbuf in
         String.sub t 1 (String.length t - 2)
     }
  | [^ '\127'-'\255' 
      '\000'-'\031'
      '(' ')' '<' '>' '@' ',' ';' ':' '\\' '"' '/' '[' ']' '?' '=' ' ' '\t']+
      { Lexing.lexeme lexbuf }
  | _ { raise (Invalid_HTTP_header "value expected") }
 
(* LWS *)
and lws = parse
   ("\r\n")? [' ' '\t']+ { () }
  | _ { raise (Invalid_HTTP_header "LWS expected")}

(* *LWS *)
and starlws = parse
   ("\r\n")? [' ' '\t']+ { starlws lexbuf }
  | "" { () }   

and realm = parse
   ['R' 'r']['E' 'e']['A' 'a']['L' 'l']['M' 'm']'='
     { quotedstring lexbuf }
 | _ { raise (Invalid_HTTP_header "realm expected") }


and authparam = parse
   ',' 
    { let _ = starlws lexbuf in
      let t = token lexbuf in
      let _ = lit_equal lexbuf in
      let qt = quotedstring lexbuf in
      let _ = starlws lexbuf in
      	(t,qt) :: authparam lexbuf
    }
 | "" { [] }

and lit_equal = parse
    '=' { () }
 |  _  { raise (Invalid_HTTP_header "= expected") }

and lit_slash = parse
    '/' { () }
 |  _  { raise (Invalid_HTTP_header "= expected") }


and media_parameters = parse
   "" { [] }
 | ";" { 
         let _ = starlws lexbuf in
         let attr = String.lowercase (token lexbuf) in
       	 let _ = lit_equal lexbuf in (* no space allowed *)
       	 let v = value lexbuf in
	 let _ = starlws lexbuf in
	 let rest = media_parameters lexbuf in
	  (attr,v)::rest
       }

and media_type = parse
   [' ' '\t']+ {
      let _ = starlws lexbuf in
      let typ = String.lowercase (token lexbuf) in
      let _ = lit_slash lexbuf in
      let subtyp = String.lowercase (token lexbuf) in
      let _ = starlws lexbuf in (* word based *)
        typ, subtyp
      }

 | "" {
      let _ = starlws lexbuf in
      let typ = String.lowercase (token lexbuf) in
      let _ = lit_slash lexbuf in
      let subtyp = String.lowercase (token lexbuf) in
      let _ = starlws lexbuf in (* word based *)
        typ, subtyp
      }

{

let media_type s =
  let lexbuf = Lexing.from_string s in
  let mtyp = media_type lexbuf in
  let l = media_parameters lexbuf in
    mtyp, l 

}

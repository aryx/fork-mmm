(*s: www/lexurl.mll *)
{
open Lexing

open Mstring
open Mlist

open Url

(*s: function Lexurl.normalize_port *)
let normalize_port = function
  | HTTP, Some 80 -> None
  | FTP, Some 21 -> None
  (* incomplete, but we don't care yet *)
  | _, p -> p
(*e: function Lexurl.normalize_port *)

(*s: function Lexurl.normalize_host *)
(* lowercase, don't use final . in FQDN *)
let normalize_host s = 
  let s = String.lowercase s in
  let l = String.length s in
  if s.[l-1] = '.' then String.sub s 0 (l-1)
  else s
(*e: function Lexurl.normalize_host *)

}

(*s: function Lexurl.f *)
(* We don't actually need all of this *)
rule f = parse
  [ 'a'-'z' 'A'-'Z' '0'-'9' '+' '.' '-' ]+ ":" 	(* absolute url *)
    { let lexeme = Lexing.lexeme lexbuf in
      let result =
        { protocol = HTTP;
          user = None; 
          password = None;
       host = None; port = None;
       path = None; search = None
         } in
      let protocol =
        String.uppercase (String.sub lexeme 0 (String.length lexeme - 1)) in
   (match protocol with
      (*s: [[Lexurl.f]] protocol cases *)
      | "HTTP" ->
         slashslash lexbuf;
         let h, po = hostport lexbuf in
         let pa, se = pathsearch lexbuf in
         result.protocol <- HTTP;
         result.host <- h;
         result.port <- normalize_port (HTTP, po);
         result.path <- pa;
         result.search <- se
      (*x: [[Lexurl.f]] protocol cases *)
      (* the spec says file://host/ dammit *)
      | "FILE" ->
        (try
           slashslash lexbuf;
           let h = fhost lexbuf in
           let p = slashpath lexbuf in
           result.protocol <- FILE;
           result.host <- h;
           result.path <- p
        with Url_Lexing _ ->
          let p = slashpath lexbuf in
          result.protocol <- FILE;
          result.path <- p
        )
      (*x: [[Lexurl.f]] protocol cases *)
      | "MAILTO" ->
          let address = any lexbuf in
          result.protocol <- MAILTO;
          result.path <- address
      (*x: [[Lexurl.f]] protocol cases *)
      | "FTP" -> (* we don't need the detail of path *)
         slashslash lexbuf;
         let u, p = userpass lexbuf in
         let h, po = hostport lexbuf in
         let path = slashpath lexbuf in
         result.protocol <- FTP;
         result.user <- u;
         result.password <- p;
         result.host <- h;
         result.port <- normalize_port (FTP, po);
         result.path <- path
      (*x: [[Lexurl.f]] protocol cases *)
      | "TELNET" ->
          slashslash lexbuf;
          let u,p = userpass lexbuf in
          let h,po = hostport lexbuf in
          result.protocol <- TELNET;
          result.user <- u;
         result.password <- p;
          result.host <- h;
         result.port <- po
      (*x: [[Lexurl.f]] protocol cases *)
      | "NNTP" ->
          let h,po = hostport lexbuf in
          let blah = any lexbuf in
          result.protocol <- NEWS;
          result.host <- h;
          result.port <- po;
          result.path <- blah
      (*x: [[Lexurl.f]] protocol cases *)
      | "GOPHER" | "GOPHER+" -> (* we don't need the detail of path *)
          slashslash lexbuf;
          let h,po = hostport lexbuf in
          let path = slashpath lexbuf in
          result.protocol <- GOPHER;
          result.host <- h;
          result.port <- po;
          result.path <- path
      | "NEWS" ->
          let blah = any lexbuf in
          result.protocol <- NEWS;
          result.path <- blah
      | "WAIS" ->
          slashslash lexbuf;
          let h,po = hostport lexbuf in
          let pa,se = pathsearch lexbuf in
          result.protocol <- WAIS;
          result.host <- h;
         result.port <- po;
          result.path <- pa;
          result.search <- se
      | "PROSPERO" ->
          slashslash lexbuf;
          let h,po = hostport lexbuf in
          let p = slashpath lexbuf in
          result.protocol <- PROSPERO;
          result.host <- h;
         result.port <- po;
          result.path <- p
      (*e: [[Lexurl.f]] protocol cases *)
      | s ->
          result.protocol <- OtherProtocol s;
          result.path <- any lexbuf
      );
      result
    }
 | _ { raise (Url_Lexing ("not an URL", Lexing.lexeme_start lexbuf)) }
(*e: function Lexurl.f *)

(*s: function Lexurl.slashslash *)
and slashslash =  parse
    "//" { () } 
   | "" { raise (Url_Lexing ("// expected", Lexing.lexeme_start lexbuf)) }
(*e: function Lexurl.slashslash *)
	
(*s: function Lexurl.userpass *)
and userpass = parse
  (* foo:bar@, foo:@ *)
  [^ ':' '/' '@']+ ':' [^ ':' '/' '@']* '@'
    { let lexeme = Lexing.lexeme lexbuf in
      let pos = String.index lexeme ':' in
      Some (String.sub lexeme 0 pos),
      Some (String.sub lexeme (succ pos) (String.length lexeme - 2 - pos))
    }
  (* foo@, @ *)
| [^ ':' '/' '@']* '@'
   { let lexeme = Lexing.lexeme lexbuf in
     Some (String.sub lexeme 0 (String.length lexeme - 1)), None
   }
      
| ""
   { None, None }
(*e: function Lexurl.userpass *)

(*s: function Lexurl.hostport *)
(* _ is not legal in hostnames, but some people use it. *)
and hostport = parse
  ['A'-'Z' 'a'-'z' '0'-'9' '.' '-' '_']+ ':' ['0'-'9']+
    { let lexeme = Lexing.lexeme lexbuf in
      let pos = String.index lexeme ':' in
      let portstring =
        String.sub lexeme (succ pos) (String.length lexeme - 1 - pos) in
   Some (normalize_host (String.sub lexeme 0 pos)),
   Some (int_of_string portstring)
    }
| ['A'-'Z' 'a'-'z' '0'-'9' '.' '-' '_']+ 
    { Some (normalize_host (Lexing.lexeme lexbuf)), None }
|  "" (* file:///home/... *)
    { None, None }
(*e: function Lexurl.hostport *)

(*s: function Lexurl.pathsearch *)
(* /<path>?<search> *)
and pathsearch = parse
  "/" [^ '?']* '?' 
    { let lexeme = Lexing.lexeme lexbuf in
      let search = any lexbuf in
      Some (String.sub lexeme 1 (String.length lexeme - 2)),
   search 
    }
| "/" [^ '?']*
    { let lexeme = Lexing.lexeme lexbuf in
      Some (String.sub lexeme 1 (String.length lexeme - 1)), None 
    }
| "" 
    { None, None }
(*e: function Lexurl.pathsearch *)

(*s: functions Lexurl.xxx *)
and any = parse
  [^ '\n']*  { Some (Lexing.lexeme lexbuf) }    (* in fact any char *)
(*x: functions Lexurl.xxx *)
and pathcomponents = parse
  [ ^ '/']* '/'
    { (function l ->
         let newl = 
           match Lexing.lexeme lexbuf with
          | "./" -> l
           | "../" -> 
              (match l with 
              | [] -> [] 
              | _ :: tl -> tl
              )
           | p -> (p :: l)
          in
       pathcomponents lexbuf newl)
     }
| [ ^ '/']+
    { (fun l -> 
     match Lexing.lexeme lexbuf with
     | "." -> l
        | ".." -> (match l with [] -> [] | _ :: tl -> tl)
        | p -> p :: l )
    }
| "" { (fun l -> l) }
(*x: functions Lexurl.xxx *)
and fhost = parse
  ['A'-'Z' 'a'-'z' '0'-'9' '.' '-']+
     { Some (normalize_host (Lexing.lexeme lexbuf)) }
| ""
     { Some "localhost" }
(*x: functions Lexurl.xxx *)
and slashpath = parse
  "/" { any lexbuf }
| ""  { None }
(*e: functions Lexurl.xxx *)

{

(*s: function Lexurl.make *)
let make s = 
  f (Lexing.from_string s)
(*e: function Lexurl.make *)



(*s: function Lexurl.remove_dots *)
let remove_dots s =
  let b = Ebuffer.create 32 in
  rev_do_list 
    (Ebuffer.output_string b)
    (pathcomponents (Lexing.from_string s) []);
  Ebuffer.get b
(*e: function Lexurl.remove_dots *)

(*s: function Lexurl.maken *)
(* Extra normalisation at lexing time 
 *  remove ../ and /. as in RFC 1630
 *  unquote %
 *)
let maken s =
  let url = make s in
  (match url.protocol with
  | HTTP ->  
      (match url.path with
      | None -> ()
      | Some p -> url.path <- Some (Urlenc.unquote (remove_dots p))
      )
  | _ -> ()
  );
  url
(*e: function Lexurl.maken *)

(*s: function Lexurl.normalize *)
let normalize url =
  let urlp = make url in
  Url.string_of urlp
(*e: function Lexurl.normalize *)

}
(*e: www/lexurl.mll *)

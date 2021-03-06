(*s: ./www/url.ml *)
open Printf

(*s: type Url.protocol *)
type protocol =
 | HTTP 
 | FILE | FTP
 | MAILTO | NNTP
 | GOPHER | NEWS | WAIS | PROSPERO
 | TELNET 
 | OtherProtocol of string
(*e: type Url.protocol *)

(*s: function Url.string_of_protocol *)
let string_of_protocol = function
   FTP -> "ftp"
 | HTTP -> "http"
 | GOPHER -> "gopher"
 | MAILTO -> "mailto"
 | NEWS -> "news"
 | NNTP -> "nntp"
 | TELNET -> "telnet"
 | WAIS -> "wait"
 | FILE -> "file"
 | PROSPERO -> "prospero"
 | OtherProtocol s -> s
(*e: function Url.string_of_protocol *)


(*s: type Url.t *)
(* URLs as defined by RFC 1738 *)

(* Not all components are used for all protocols. See RFC. *)
type t = 
  { mutable protocol : protocol;

    mutable user : string option;
    mutable password: string option;

    mutable host : string option;
    mutable port : int option;

    mutable path : string option;
    mutable search: string option
  }
(*e: type Url.t *)

(*s: exception Url.Url_Lexing *)
exception Url_Lexing of string * int
(*e: exception Url.Url_Lexing *)
(*s: exception Url.Invalid_url *)
exception Invalid_url of t * string
(*e: exception Url.Invalid_url *)

(*s: function Url.string_of *)
let string_of p =
  let buf = Ebuffer.create 128 in
  let ws x = Ebuffer.output_string buf x in
  let wc x = Ebuffer.output_char buf x in
  let write_userpass () =
      match p.user, p.password with
         None, None -> ()
       | Some u, Some p -> ws u; wc ':'; ws p; wc '@'
       | Some u, None ->   ws u; wc ':'; wc '@'
       | None, Some _ -> failwith "url_of_parsed"
  in
  (* hostname is always put in lowercase *)
  let write_hostport def =
      match p.host, p.port with
         None, None -> ()
       | Some h, None -> ws (String.lowercase h)
       | Some h, Some p when p = def -> ws (String.lowercase h)
       | Some h, Some p -> 
           ws (String.lowercase h); wc ':'; ws (string_of_int p)
       | None, Some _ -> failwith "url_of_parsed"	    
  in
  let write_pathsearch () =
      match p.path, p.search with
        None, None -> wc '/'
      | Some p, Some s -> wc '/'; ws p; wc '?'; ws s
      | Some p, None -> wc '/'; ws p
      | None, Some _ -> failwith "url_of_parsed"	    
  in
  let write_slashpath () =
      match p.path with
       None -> ()
      | Some p -> wc '/'; ws p
  in
  let write_path () =
      match p.path with
       None -> ()
      | Some p -> ws p
  in
  let write_fhost () =
      match p.host with
       None -> ws "localhost"
      | Some h -> ws (String.lowercase h)
  in
  begin match p.protocol with
    FTP ->
      ws "ftp://"; write_userpass (); write_hostport 21; write_slashpath ()
  | HTTP ->
      ws "http://"; write_hostport 80; write_pathsearch ()
  | GOPHER ->
      ws "gopher://"; write_hostport 70; write_slashpath ()
  | MAILTO -> ws "mailto:"; write_path()
  | NEWS -> ws "news:"; write_path()
  | NNTP -> ws "nntp:"; write_hostport 119; write_path()
  | TELNET -> ws "telnet://"; write_userpass(); write_hostport 23
  | WAIS -> ws "wais://"; write_hostport 210; write_pathsearch()
  | FILE ->
    (* for file: we have to transform to ftp: if host is not localhost *)
    begin match p.host with
      None | Some "localhost" ->
        ws "file://"; write_fhost(); write_slashpath()
    | Some h ->
       p.protocol <- FTP;
        ws "ftp://"; write_userpass (); write_hostport 21; write_slashpath ()
    end
  | PROSPERO -> ws "prospero://"; write_hostport 1525; write_slashpath()
  | OtherProtocol s -> ws s; ws ":"; write_path()
  end;
  Ebuffer.get buf
(*e: function Url.string_of *)

(*s: function Url.distant_path *)
(* For http only *)
let distant_path urlp =
  match urlp.path, urlp.search with
     None, None -> "/"
   | Some p, None -> "/"^p
   | Some p, Some s -> "/" ^ p ^ "?" ^ s
   | None, Some s -> "/?" ^ s (* ??? *)
(*e: function Url.distant_path *)

(*e: ./www/url.ml *)

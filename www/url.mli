(* URLs as defined by RFC 1738 *)

type protocol =
   FTP | HTTP | GOPHER | MAILTO | NEWS | NNTP | TELNET | WAIS
 | FILE | PROSPERO
 | OtherProtocol of string

val string_of_protocol: protocol -> string
  (* maps FTP to "ftp", etc... *)

(* Not all components are used for all protocols. See RFC. *)
(* Relative adressing in anchors, fragments are NOT URLs, but URI *)
type t = 
  { mutable protocol : protocol;
    mutable user : string option;
    mutable password: string option;
    mutable host : string option;
    mutable port : int option;
    mutable path : string option;
    mutable search: string option
  }

(* These are used to get "normalized urls" *)
val string_of: t -> string

(* For http. The thing we have to send in the request *)
val distant_path : t -> string

exception Url_Lexing of string * int



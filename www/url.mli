(*s: ./www/url.mli *)
(*s: type Url.protocol *)
(* URLs as defined by RFC 1738 *)

type protocol =
 | HTTP 
 | FILE | MAILTO | FTP
 | GOPHER | NEWS | NNTP | WAIS
 | TELNET 
 | PROSPERO
 | OtherProtocol of string
(*e: type Url.protocol *)

(*s: signature Url.string_of_protocol *)
val string_of_protocol: protocol -> string
  (* maps FTP to "ftp", etc... *)
(*e: signature Url.string_of_protocol *)

(* Not all components are used for all protocols. See RFC. *)
(*s: type Url.t *)
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
(*e: type Url.t *)

(*s: signature Url.string_of *)
(* These are used to get "normalized urls" *)
val string_of: t -> string
(*e: signature Url.string_of *)

(*s: signature Url.distant_path *)
(* For http. The thing we have to send in the request *)
val distant_path : t -> string
(*e: signature Url.distant_path *)

(*s: exception Url.Url_Lexing *)
exception Url_Lexing of string * int
(*e: exception Url.Url_Lexing *)


(*e: ./www/url.mli *)

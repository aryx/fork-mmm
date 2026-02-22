(*s: www/url.mli *)
(*s: type [[Url.protocol]] *)
type protocol =
 | HTTP 
 | FILE | FTP
 | MAILTO | NNTP
 | GOPHER | NEWS | WAIS | PROSPERO
 | TELNET 
 | OtherProtocol of string
(*e: type [[Url.protocol]] *)

(*s: signature [[Url.string_of_protocol]] *)
val string_of_protocol: protocol -> string
  (* maps FTP to "ftp", etc... *)
(*e: signature [[Url.string_of_protocol]] *)

(*s: type [[Url.t]] *)
(* URLs as defined by RFC 1738. Not all components are used for all protocols.
 * The order of the fields below correspond to the actual order in the string:
 *   <protocol>://<user>:<password>@<host>:<port>/<path>?<search>
*)
type t = 
  { mutable protocol : protocol;

    mutable user : string option;
    mutable password: string option;

    mutable host : string option;
    mutable port : int option;

    mutable path : string option;
    mutable search: string option
  }
(*e: type [[Url.t]] *)

(*s: signature [[Url.string_of]] *)
(* These are used to get "normalized urls" *)
val string_of: t -> string
(*e: signature [[Url.string_of]] *)

(*s: signature [[Url.distant_path]] *)
(* For http. The thing we have to send in the request *)
val distant_path : t -> string
(*e: signature [[Url.distant_path]] *)

(*s: exception [[Url.Url_Lexing]] *)
exception Url_Lexing of string * int
(*e: exception [[Url.Url_Lexing]] *)
(*e: www/url.mli *)

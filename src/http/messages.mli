(*s: http/messages.mli *)
(*s: copyright header v6 *)
(***********************************************************************)
(*                                                                     *)
(*                           The V6 Engine                             *)
(*                                                                     *)
(*          Francois Rouaix, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header v6 *)

(* HTTP Messages *)

(*s: type [[Messages.request]] *)
(* Request-Line of a Request *)
type request_line = {
  request_version: string;	(* HTTP/1.0 *)
  request_method : string;	(* GET, POST, etc... *)

  request_uri : string		(* the uri *)
}
(*e: type [[Messages.request]] *)

(*s: type [[Messages.status]] *)
(* Status-Line of a Response *)
type status_line =  { 
    status_version : string;	(* HTTP/1.0 *)
    status_code : int;		(* http return codes *)
    status_message : string	(* http return message *)
}
(*e: type [[Messages.status]] *)

(*s: type [[Messages.header]] *)
(* Other headers *)
type header = string
(*e: type [[Messages.header]] *)

(* HTTP messages: requests and responses
 *  What a client sends to a server is called a request 
 *  What a server answers is called a response
 *)
(*s: type [[Messages.request_message]] *)
(* HTTP-Message *)
type request = {
  request : request_line;
  request_headers : header list;
  request_body : string;
  (*s: [[Messages.request_message]] other fields *)
  request_auth : (string * string) option;
           (* have we authentified the emitter (authtype, authuser) *)
  (*e: [[Messages.request_message]] other fields *)
}
(*e: type [[Messages.request_message]] *)

(*s: type [[Messages.response_message]] *)
type response = {
  status : status_line;

  response_headers : header list;
  response_body : string;        (* responde body is *not* the document body *)
}
(*e: type [[Messages.response_message]] *)
(*e: http/messages.mli *)

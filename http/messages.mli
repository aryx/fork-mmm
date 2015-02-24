(*s: ./http/messages.mli *)
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

(* $Id: messages.mli,v 1.1 1996/10/22 13:12:36 rouaix Exp $ *)

(* HTTP Messages *)

(*s: enum Messages.request *)
(* Request-Line of a Request *)
type request = {
  request_version: string;	(* HTTP/1.0 *)
  request_method : string;	(* GET, POST, etc... *)
  request_uri : string		(* the uri *)
  }
(*e: enum Messages.request *)

(*s: enum Messages.status *)
(* Status-Line of a Response *)
type status =  { 
    status_version : string;	(* HTTP/1.0 *)
    status_code : int;		(* http return codes *)
    status_message : string	(* http return message *)
 }
(*e: enum Messages.status *)

(*s: enum Messages.header *)
(* Other headers *)
type header = string
(*e: enum Messages.header *)


(* HTTP messages: requests and responses
 *  What a client sends to a server is called a request 
 *  What a server answers is called a response
 *)

(*s: enum Messages.request_message *)
(* HTTP-Message *)
type request_message = {
  request : request;
  request_headers : header list;
  request_body : string;
  request_auth : (string * string) option 
           (* have we authentified the emitter (authtype, authuser) *)
  }
(*e: enum Messages.request_message *)

(*s: enum Messages.response_message *)
type response_message = {
  status : status;
  response_headers : header list;
  response_body : string        (* responde body is *not* the document body *)
  }
(*e: enum Messages.response_message *)

(*e: ./http/messages.mli *)

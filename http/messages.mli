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

(* Request-Line of a Request *)
type request = {
  request_version: string;	(* HTTP/1.0 *)
  request_method : string;	(* GET, POST, etc... *)
  request_uri : string		(* the uri *)
  }

(* Status-Line of a Response *)
type status =  { 
    status_version : string;	(* HTTP/1.0 *)
    status_code : int;		(* http return codes *)
    status_message : string	(* http return message *)
 }

(* Other headers *)
type header = string


(* HTTP messages: requests and responses
 *  What a client sends to a server is called a request 
 *  What a server answers is called a response
 *)

(* HTTP-Message *)
type request_message = {
  request : request;
  request_headers : header list;
  request_body : string;
  request_auth : (string * string) option 
      	    (* have we authentified the emitter (authtype, authuser) *)
  }

type response_message = {
  status : status;
  response_headers : header list;
  response_body : string        (* responde body is *not* the document body *)
  }


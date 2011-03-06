{
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

(* $Id: lexpath.mll,v 1.1 1996/10/22 13:12:47 rouaix Exp $ *)
}

rule f  = parse
    [ ^ '/']+ '/'
      	{ (fun l ->
	    let newl = match Lexing.lexeme lexbuf with
	       "./" -> l
	     | "../" -> (match l with [] -> [] | _ :: tl -> tl)
	     | p ->  (String.sub p 0 (String.length p - 1)) :: l
	     in
	    f lexbuf newl)
      	}
  | "/" { (fun l -> f lexbuf l) }
  |  [ ^ '/']+
      	{ (fun l -> 
	     match Lexing.lexeme lexbuf with
	       "." -> l
             | ".." -> (match l with [] -> [] | _ :: tl -> tl)
             | p -> p :: l )}
  | "" {(fun l -> l)}

{
 let rev_path_components lexbuf = f lexbuf []

 let path_components lexbuf = List.rev (f lexbuf [])

 (* Build a unix path from path components *)
 let rec build s = function
     [] -> s
   | x::l -> build (Filename.concat s x) l

 let remove_dots s =
    build "/" (path_components (Lexing.from_string s))

}

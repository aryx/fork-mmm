(*s: globals/version.ml *)
(* To merge FR and JP strings correctly, you have to encode the characters
 * more than 0x7F, for example "Fran\231ois".
 *) 

(*s: constant [[Version.number]] *)
(* Version *)
let number = 418
(*e: constant [[Version.number]] *)
(*s: constant [[Version.version_number]] *)
let version_number = 
  string_of_int number
(*e: constant [[Version.version_number]] *)

(*s: constant [[Version.http]] *)
(* User-Agent field *)
let http = "MMM/0." ^ string_of_int number
(*e: constant [[Version.http]] *)

(*s: function [[Version.about]] *)
(* dialog uses an gigantic font ! *)
let about = function
  | "iso8859" ->
"MMM Version 0." ^ version_number ^
"\nWritten by Fran\231ois Rouaix
Contributions by Jun P. Furuse and Jacques Garrigue
Ported to O'Caml 3 by Jun P. Furuse and Pierre Weis
\169 Copyright INRIA

Projet Cristal
INRIA Rocquencourt
Domaine de Voluceau
78153 Le Chesnay Cedex
France

Francois.Rouaix@inria.fr
http://pauillac.inria.fr/~rouaix/
"
  | s -> failwith (Printf.sprintf "language not supported: %s" s)
(*e: function [[Version.about]] *)

(*s: function [[Version.home]] *)
let home_mmm = function 
  | "iso8859" -> "http://pauillac.inria.fr/mmm/"
  | _ -> assert false
(*e: function [[Version.home]] *)

(*s: function [[Version.initurl]] *)
(* MUST BE NORMALIZED *)
let initurl = function
  | "iso8859" -> 
      Printf.sprintf "http://pauillac.inria.fr/mmm/v%d/about.html" number
  | s -> failwith (Printf.sprintf "language not supported: %s" s)
(*e: function [[Version.initurl]] *)

(*s: function [[Version.helpurl]] *)
let helpurl = function
  | "iso8859" -> 
      Printf.sprintf "http://pauillac.inria.fr/mmm/v%d/docindex.html" number
  | _ -> assert false
(*e: function [[Version.helpurl]] *)

(*s: function [[Version.html]] *)
let inithtml = function
  | "iso8859" ->
"<HTML>
  <HEAD><TITLE>MMM 0." ^ version_number ^ "</TITLE></HEAD>
<BODY>
<H1> The MMM navigator Version 0." ^ version_number ^ "</H1>
<H2 ALIGN=CENTER> Written by Fran\231ois Rouaix </H2>
<H2 ALIGN=CENTER> Contributions by Jun P. Furuse and Jacques Garrigue</H2>
<H3 ALIGN=CENTER> Port to O'Caml V3.0 by Jun P. Furuse and Pierre Weis</H3>
<H2 ALIGN=CENTER> \169 Copyright INRIA </H2>

<H4 ALIGN=CENTER> Using Objective Caml \169 Copyright INRIA </H4>
<H4 ALIGN=CENTER> And Tcl8.0/Tk8.0 (John Ousterhout and al.)<BR>
 \169 Copyright The Regents of the University of California<BR>
 and Sun Microsystems, Inc </H4>
<BLOCKQUOTE>
Please note that the software is a product currently being developed.
INRIA shall not be responsible in any way concerning conformity, and in
particular shall not be liable should the software not comply with the
requirements of the user, INRIA not being obliged to repair any
possible direct or indirect damage.
</BLOCKQUOTE>
<P>
The MMM home page is 
<A HREF='http://pauillac.inria.fr/mmm/'>here</A>,
and there is also some
<A HREF='http://pauillac.inria.fr/mmm/doc.html'>documentation</A>
and
<A HREF='http://pauillac.inria.fr/mmm/releases.html'>release notes</A>.
<BR>
Join the author by clicking
<A HREF='mailto:Francois.Rouaix@inria.fr'>here.</A>
<P>
<BLOCKQUOTE>
This document is included in your browser. Click on <TT>Reload</TT> to
get an updated copy.
</BLOCKQUOTE>
</BODY>
</HTML>
"
  |  s -> failwith (Printf.sprintf "language %s not supported here" s)
(*e: function [[Version.html]] *)
(*e: globals/version.ml *)

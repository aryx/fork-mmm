(*s: ./globals/version.ml *)
(* To merge FR and JP strings correctly, you have to encode the characters
 * more than 0x7F, for example "Fran\231ois".
 *) 

(*s: constant Version.number *)
(* Version *)
let number = 418
(*e: constant Version.number *)
(*s: constant Version.version_number *)
let version_number = string_of_int number;;
(*e: constant Version.version_number *)

(*s: constant Version.http *)
(* User-Agent field *)
let http = "MMM/0." ^ string_of_int number
(*e: constant Version.http *)

(*s: function Version.about *)
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
  | "ja" -> "\027$B$b!A\027(B Version 0." ^ version_number ^
"\n\027$B:n<T\027(B: Fran\231ois Rouaix
\027$BF|K\\8l2=<T\027(B: \027$B8E@%\027(B \027$B=_\027(B
\027$B4sM?\027(B: \027$B2mMx2l\027(B \027$B<f6j\027(B
O'Caml 3 \027$B$X$N0\\?\"\027(B: \027$B8E@%\027(B \027$B=_!\"\027(B Pierre Weis
\169 Copyright INRIA

Projet Cristal
INRIA Rocquencourt
Domaine de Voluceau
78153 Le Chesnay Cedex
France

Francois.Rouaix@inria.fr
http://pauillac.inria.fr/~rouaix/

Jun.Furuse@inria.fr
http://pauillac.inria.fr/~furuse/
"
  | _ -> assert false
(*e: function Version.about *)

(*s: function Version.home *)
let home = function 
  | "iso8859" -> "http://pauillac.inria.fr/mmm/"
  | "ja" -> "http://pauillac.inria.fr/mmm/jmmm/"
  | _ -> assert false
(*e: function Version.home *)

(*s: function Version.initurl *)
(* MUST BE NORMALIZED *)
let initurl = function
  | "iso8859" -> 
      Printf.sprintf "http://pauillac.inria.fr/mmm/v%d/about.html" number
  | "ja" -> 
      Printf.sprintf "http://pauillac.inria.fr/mmm/jmmm/v%d/about.html"
    number
  | _ -> assert false
(*e: function Version.initurl *)

(*s: function Version.helpurl *)
let helpurl = function
  | "iso8859" -> 
      Printf.sprintf "http://pauillac.inria.fr/mmm/v%d/docindex.html" number
  | "ja" -> 
      Printf.sprintf "http://pauillac.inria.fr/mmm/v%d/docindex-ja.html"
    number
  | _ -> assert false
(*e: function Version.helpurl *)

(*s: function Version.html *)
let html = function
  | "iso8859" ->
"<HTML><HEAD><TITLE>MMM 0." ^ version_number ^ "</TITLE></HEAD>
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
<A HREF=\"http://pauillac.inria.fr/mmm/\">here</A>,
and there is also some
<A HREF=\"http://pauillac.inria.fr/mmm/doc.html\">documentation</A>
and
<A HREF=\"http://pauillac.inria.fr/mmm/releases.html\">release notes</A>.
<BR>
Join the author by clicking
<A HREF=\"mailto:Francois.Rouaix@inria.fr\">here.</A>
<P>
<BLOCKQUOTE>
This document is included in your browser. Click on <TT>Reload</TT> to
get an updated copy.
</BLOCKQUOTE>
</BODY>
</HTML>
"
| "ja" -> "<HTML><HEAD><TITLE>MMM 0." ^ version_number ^ "</TITLE></HEAD>
<BODY>
<H1> \027$B%J%S%2!<%?\027(B \027$B$b!A\027(B Version 0." ^ version_number ^ "</H1>
<H2> \027$B:n<T\027(B \027$B!'\027(B Fran&ccedil;ois Rouaix </H2>
<H2> \027$BF|K\\8l2=<T\027(B \027$B!'\027(B \027$B8E@%\027(B \027$B=_\027(B </H2>
<H2> \027$B4sM?\027(B \027$B!'\027(B \027$B2mMx2l\027(B \027$B<f6j\027(B </H2>
<H2> O'Caml 3 \027$B$X$N0\\?\"\027(B: \027$B8E@%\027(B \027$B=_!\"\027(B Pierre Weis </H2>
<H2> &copy; Copyright INRIA (\027$BJ)9q9qN)>pJs<+F02=8&5f=j\027(B) </H2>

<H3> \027$B;HMQ%=%U%H%&%'%\"\027(B </H3>
<H3> Objective Caml &copy; Copyright INRIA </H3>
<H3> Tcl8.0/Tk8.0 (John Ousterhout and al.) &copy; Copyright The Regents of the University of California and Sun Microsystems, Inc </H3>
<BLOCKQUOTE>
\027$B$3$N%=%U%H%&%'%\"$K4X$7$F\027(BINRIA \027$B$O$$$+$J$k7A<0$N@UG$$bIi$$$^$;$s!#\027(B
\027$BFC$K!\"$3$N%=%U%H%&%'%\"$r%f!<%6!<$NI,MW$KJ;$;$k5AL3!\"\027(B
\027$B$^$?;HMQ$7$F@8$8$?$$$+$J$kD>@\\E*$^$?$O4V@\\E*Ho32$N@UG$$bIi$$$+$M$^$9!#\027(B
</BLOCKQUOTE>
<P>
\027$B$b!A$N%[!<%`%Z!<%8$O\027(B
<A HREF=\"http://pauillac.inria.fr/mmm/jmmm/\">\027$B$3$3\027(B</A>\027$B!\"\027(B
\027$B$b!A$N1Q8l$G=q$+$l$?%*%j%8%J%k$N%[!<%`%Z!<%8$O\027(B
<A HREF=\"http://pauillac.inria.fr/mmm/\">\027$B$3$3\027(B</A>\027$B$G$9!#\027(B
<BR>
\027$B6=L#$,$\"$kJ}$O\027(B
<A HREF=\"mailto:Jun.Furuse@inria.fr\">\027$B$3$3\027(B</A>
\027$B$r2!$7$F3+H/$K;22C$7$F2<$5$$!#\027(B
<P>
<BLOCKQUOTE>
\027$B$3$NJ8=q$O%V%i%&%6$KFbB\"$5$l$F$$$^$9!#\027(B<TT>\027$B:FFI\027(B</TT> \027$B$r2!$7$F0lHV?7$7$$J*$r<hF@$7$F2<$5$$!#\027(B
</BLOCKQUOTE>
</BODY>
</HTML>
"
  |  _ -> assert false
(*e: function Version.html *)

(*s: constant Version.applet_init *)
(* Make it easier to compile both bytecode and native versions *)
let applet_init = ref (fun _ -> ())
(*e: constant Version.applet_init *)
(*e: ./globals/version.ml *)

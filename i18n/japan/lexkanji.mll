{
open Lexing
open Charset

exception MustbeEUC
exception MustbeSJIS
exception Lexkanji_Error of string

type lexdata = {
    stored : Wstream.t;
    unresolved : Ebuffer.t
  } 

let lexeme_sub lexbuf start code size =
  let str = String.sub (lexeme lexbuf) start 
    (String.length (lexeme lexbuf) - start) in
  let len = String.length str / size in
  let a = Array.create len (code, "") in
  for i = 0 to len - 1 do
    a.(i) <- code, String.sub str (i * size) size
  done;
  a

let ascii_array s =
  let a = Array.create (String.length s) (ASCII, "") in
  for i = 0 to String.length s - 1 do
    a.(i) <- ASCII, String.make 1 s.[i]
  done;
  a

let newdata () = {
  stored = Wstream.create 100;
  unresolved = Ebuffer.create 100
} 

let debug s = 
  prerr_string s;
  prerr_string (Printf.sprintf " %d " (String.length s));
  prerr_string "<<";
  for i = 0 to String.length s - 1 do
    prerr_string (Printf.sprintf "%03d " (Char.code s.[i]))
  done;
  prerr_endline ">>"
} 

rule ascii = parse
   ['\000' - '\026' '\028' - '\127']+
   { ( fun lexdata ->
       Wstream.output_array lexdata.stored (ascii_array (lexeme lexbuf)) )}
 | eof 
   { ( fun lexdata ->
       raise End_of_file )}

and iso8859 = parse
   (* ISO8859 *)
   ['\000' - '\026' '\028' - '\127' '\161' - '\255']+
   { ( fun lexdata ->
       Wstream.output_array lexdata.stored (ascii_array (lexeme lexbuf)) )}
 | eof 
   { ( fun lexdata ->
       raise End_of_file )}

and junet = parse
   (* ISO2022 ENCODEINGS *)
   "\027(" ['A'-'C'] [^ '\027' '\128'-'\159']* (* Latin 1,2,3 *)
    { ( fun lexdata ->
        Wstream.output_array lexdata.stored 
	(lexeme_sub lexbuf 3 ASCII 1))}
 | "\027(J" ['\033' - '\126']* (* JISX0201Roman *)
    { ( fun lexdata ->
        Wstream.output_array lexdata.stored 
	  (lexeme_sub lexbuf 3 JISX0201_Roman 1) )}
 | "\027(I" ['\033' - '\126']* (* JISX0201Katakana *)
    { ( fun lexdata ->
        Wstream.output_array lexdata.stored 
          (lexeme_sub lexbuf 3 JISX0201_Katakana 1) )}
 | "\027$B" ([ '\033'-'\126' ] [ '\033'-'\126' ])* (* JISX0208_1983 *)
    { ( fun lexdata ->
        Wstream.output_array lexdata.stored 
          (lexeme_sub lexbuf 3 JISX0208_1983 2) )}
 | "\027$(B" ([ '\033'-'\126' ] [ '\033'-'\126' ])* (* JISX0208_1983 *)
    { ( fun lexdata ->
        Wstream.output_array lexdata.stored 
          (lexeme_sub lexbuf 4 JISX0208_1983 2) )}
 | "\027$@" ([ '\033'-'\126' ] [ '\033'-'\126' ])*
    { ( fun lexdata ->
        Wstream.output_array lexdata.stored 
          (lexeme_sub lexbuf 3 JISX0208_1978 2) )}
 | "\027$(@" ([ '\033'-'\126' ] [ '\033'-'\126' ])*
    { ( fun lexdata ->
        Wstream.output_array lexdata.stored 
          (lexeme_sub lexbuf 4 JISX0208_1978 2) )}
 | "\027$(D" ([ '\033'-'\126' ] [ '\033'-'\126' ])* (* JISX0212_1990 *)
    { ( fun lexdata ->
        Wstream.output_array lexdata.stored 
          (lexeme_sub lexbuf 4 JISX0212_1990 2) )}
 | "\027" ['\032' - '\047'] ['\032' - '\047']? ['\048'-'\127']  
    { ( fun lexdata ->
        let seq = lexeme lexbuf in
	prerr_endline ("Unfamiliar ESC seq : ESC" ^
				String.sub seq 1 (String.length seq - 1));
	Wstream.output_array lexdata.stored (ascii_array (lexeme lexbuf)) )}
 | ['\001' - '\026' '\028' - '\127']
    { ( fun lexdata -> 
	Wstream.output_one lexdata.stored (ASCII, lexeme lexbuf) )}
 | eof {( fun lexdata ->
          raise End_of_file )}

and eucjapan = parse
   ['\001' - '\026' '\028' - '\127']+
   { ( fun lexdata ->
       Wstream.output_array lexdata.stored (ascii_array (lexeme lexbuf)) )}
 | ['\161' - '\254'] ['\161' - '\254'] (* Characters (right) *)
    { ( fun lexdata ->
        let buf = String.create 2 in
        let src = lexeme lexbuf in
        buf.[0] <- Char.chr (Char.code src.[0] - 128);
        buf.[1] <- Char.chr (Char.code src.[1] - 128);
        Wstream.output_one lexdata.stored (JISX0208_1983, buf) )}
 | ('\142' ['\161' - '\254'])+ (* "hankaku" katakana *)
   { ( fun lexdata ->
       let src = lexeme lexbuf in 
       for i = 0 to String.length src / 2 - 1 do
	 Wstream.output_one lexdata.stored 
	   (JISX0201_Katakana, String.make 1 (Char.chr (
	      Char.code src.[i * 2 + 1] - 128)))
       done )}
 | '\143' ['\161' - '\254'] ['\161' - '\254'] (* JISX0212_1990 *)
   { ( fun lexdata ->
       let buf = String.create 2 in
       let src = lexeme lexbuf in
       buf.[0] <- Char.chr (Char.code src.[1] - 128);
       buf.[1] <- Char.chr (Char.code src.[2] - 128);
       Wstream.output_one lexdata.stored (JISX0212_1990, buf) )}
 | eof {( fun lexdata ->
          raise End_of_file )}

and sjis = parse
   ['\001' - '\026' '\028' - '\127']+
   { ( fun lexdata ->
       Wstream.output_array lexdata.stored (ascii_array (lexeme lexbuf)) )}
 | ['\161' - '\223']+ (* KANA 10/1 - 13/15 *)
   { ( fun lexdata ->
       let src = lexeme lexbuf in
       for i = 0 to String.length src - 1 do
       Wstream.output_one lexdata.stored
         (JISX0201_Katakana, String.make 1 (Char.chr (
	    Char.code src.[i] - 128)))
       done )}
 | ['\129' - '\159' '\224' - '\250'] ['\064' - '\126' '\128' - '\252']
   (* 8/1 - 9/15, 14/0 - 15/10 :: 4/0 - 15/12 (except 7/15) *) 
   { ( fun lexdata ->  
       let src = lexeme lexbuf in
       let dst = String.create 2 in
       try
  	 let ku, ten = 
  	   match Char.code src.[0], Char.code src.[1] with
  	     c, c2 when c >= 240 -> (* f$%^ing GAIJI *)
  	       raise Exit
  	   | c, c2 when c <= 159 && c2 < 159 ->
  	       c * 2 - 257, if c2 <= 126 then c2 - 63 else c2 - 64
  	   | c, c2 when c <= 159 && c2 >= 159 ->
  	       c * 2 - 256, c2 - 158 
  	   | c, c2 when 224 <= c && c2 < 159 ->
  	       c * 2 - 385, if c2 <= 126 then c2 - 63 else c2 - 64
  	   | c, c2 when 224 <= c && c >= 159 ->
  	       c * 2 - 384, c2 - 158 
  	   | _, _ -> raise (Lexkanji_Error "Lexkanji.sjis")
	 in
	   dst.[0] <- Char.chr (ku + 32);
	   dst.[1] <- Char.chr (ten + 32);
	   Wstream.output_one lexdata.stored (JISX0208_1983, dst)
       with
	 Exit -> Wstream.output_array lexdata.stored (ascii_array "[GAIJI]") )}
 | eof {( fun lexdata ->
          raise End_of_file )}

and eucorsjis = parse
   ['\001' - '\026' '\028' - '\127']+
   { ( fun lexdata ->
       Ebuffer.output_string lexdata.unresolved (lexeme lexbuf);
       eucorsjis lexbuf lexdata )}
 | ['\129' - '\141' '\144' - '\159'] ['\064' - '\126' '\128' - '\252'] (*SJIS*)
   { ( fun lexdata ->
       Ebuffer.output_string lexdata.unresolved (lexeme lexbuf);
       raise MustbeSJIS )}
 | ['\224' - '\250'] ['\064' - '\160'] (* SJIS *)
   { ( fun lexdata ->
       Ebuffer.output_string lexdata.unresolved (lexeme lexbuf);
       raise MustbeSJIS )}
 | ['\250' - '\254'] ['\161' - '\254'] (* EUC *)
   { ( fun lexdata ->
       Ebuffer.output_string lexdata.unresolved (lexeme lexbuf);
       raise MustbeEUC )}
 (* KANA (SJIS) *) 
 | ['\161' - '\223'] ['\161' - '\254'] 
   (* EUC or SJIS  : the first one can be SJIS Kana but we give up *)
   { ( fun lexdata ->
       Ebuffer.output_string lexdata.unresolved (lexeme lexbuf);
       raise MustbeEUC )}
 | ['\161' - '\223'] (* SJIS KANA / EUC *)  
   { ( fun lexdata ->
       Ebuffer.output_string lexdata.unresolved (lexeme lexbuf);
       raise MustbeSJIS )}
 (* SS2 (EUC) *)  
 | '\142' ['\253' - '\254'] (* "hankaku" katakana in EUC *)
   { ( fun lexdata ->
       Ebuffer.output_string lexdata.unresolved (lexeme lexbuf);
       raise MustbeEUC )}
 | '\142' ['\161' - '\252'] (* "hankaku" katakana in EUC / SJIS Kanji *)
   { ( fun lexdata ->
       Ebuffer.output_string lexdata.unresolved (lexeme lexbuf) )}
 | '\142' ['\064' - '\126' '\128' - '\160'] (* remained case must be SJIS *)
   { ( fun lexdata ->
       Ebuffer.output_string lexdata.unresolved (lexeme lexbuf);
       raise MustbeSJIS )}
 (* SS3 (EUC) *)  
 | '\143' (['\253' - '\254'] ['\161' - '\254'] |
           ['\161' - '\254'] ['\253' - '\254'] ) (* JISX0212_1990 in EUC *)
   { ( fun lexdata ->
       Ebuffer.output_string lexdata.unresolved (lexeme lexbuf);
       raise MustbeEUC )}
 | '\143' ['\161' - '\252'] ['\161' - '\252'] 
   (* JISX0212_1990 in EUC or SJIS, but we give up about SJIS *)
   { ( fun lexdata ->
       Ebuffer.output_string lexdata.unresolved (lexeme lexbuf);
       raise MustbeEUC )}
 | '\143' ['\064' - '\126' '\128' - '\252'] (* remained cases must be SJIS *)
   { ( fun lexdata ->
       Ebuffer.output_string lexdata.unresolved (lexeme lexbuf);
       raise MustbeSJIS )}
 (* KANJI *) 
 | ['\224' - '\250'] ['\161' - '\252'] 
   { ( fun lexdata ->
       Ebuffer.output_string lexdata.unresolved (lexeme lexbuf);
       eucorsjis lexbuf lexdata )}

 (* Debug : EUC *)
 | (['\161' - '\254'] ['\161' - '\254'] |
    '\142' ['\161' - '\254'] |
    '\143' ['\161' - '\254'] ['\161' - '\254'])
   { ( fun lexdata -> raise (Failure "eucorsjis: EUC is not exterminated") )}

 (* Debug : SJIS *)
 | ( ['\161' - '\223'] |
     ['\129' - '\159' '\224' - '\250'] ['\064' - '\126' '\128' - '\252'] )
   { ( fun lexdata -> raise (Failure "eucorsjis: SJIS is not exterminated") )}

 | eof {( fun lexdata ->
          raise End_of_file )}
  
and remove_garbage = parse
   ['\001' - '\026' '\028' - '\127']
   { ( fun lexdata ->
       Wstream.output_one lexdata.stored (ASCII, lexeme lexbuf) )}
 | ['\027' '\128' - '\255']
   { ( fun lexdata ->
       Wstream.output_one lexdata.stored (ASCII, "#")) }
 | eof 
   { ( fun lexdata ->
       raise End_of_file )}


and length = parse
  | "\027\040\066" (* ASCII *) 
      {( fun len ->
	length lexbuf len )}
  | "\027\036" ['\064' - '\068'] ['\033' - '\126']+
      {( fun len ->
	let add = (String.length (Lexing.lexeme lexbuf) - 3) / 2 in 
	length lexbuf (len + add) )}
  | "\027\036\040" ['\064' - '\068'] ['\033' - '\126']+
      {( fun len ->
	let add = (String.length (Lexing.lexeme lexbuf) - 4) / 2 in 
	length lexbuf (len + add) )}
  | "\027\036\040" ['\064' - '\068'] ['\033' - '\126']+
      {( fun len ->
	let add = (String.length (Lexing.lexeme lexbuf) - 4) / 2 in 
	length lexbuf (len + add) )}
  | _ {( fun len -> length lexbuf (len + 1) )}
  | eof {( fun len -> len )}

{
let length str =
  let lexbuf = Lexing.from_string str in
  length lexbuf 0
} 

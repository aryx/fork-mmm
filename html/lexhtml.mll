(* An HTML lexer *)
{
open Html

type tagtoken =
   Attribute of string * string
 | Closetag of int
 | Bogus of string * int   (* Bogus(s,n) == bug at pos [n] for reason [s] *)

(* Smart hack to make lexers reentrant.
 * Make each action a function taking "private" data as argument.
 * Invoke each action with additionnal argument.
 *  
 * This works only because calls to actions in csllex generated code
 * are terminal.
 *)

type t = {
  buffer : Ebuffer.t;
  mutable start : int;
  mutable pos_fix : int
  }

let new_data () = {
  buffer = Ebuffer.create 512;
  start = 0;
  pos_fix = 0 
  }

let strict = ref false

type warnings = (string * int) list
}

rule html = parse
 | '\n'? "</"
    { (fun lexdata ->
	 lexdata.start <- Lexing.lexeme_start lexbuf - lexdata.pos_fix;
	 closetag lexbuf lexdata
      )}
 | '<' 
    { (fun lexdata ->
	 lexdata.start <- Lexing.lexeme_start lexbuf - lexdata.pos_fix;
	 opentag lexbuf lexdata
      )}
 | "<!>" 
    { (fun lexdata ->
           [],
	   Comment "",
      	   Loc (Lexing.lexeme_start lexbuf - lexdata.pos_fix, Lexing.lexeme_end lexbuf - lexdata.pos_fix)
       )}
(* If you think it is possible to deal with malformed comments adaptatively,
   that is switching to lenient mode only after we detected an error
   in comment syntax, then ponder the following example: <!-- -- --> *)
 | "<!--"
    { (fun lexdata ->
      lexdata.start <- Lexing.lexeme_start lexbuf - lexdata.pos_fix;
      Ebuffer.reset lexdata.buffer;
      if !strict then comment lexbuf lexdata
      else lenient_end_comment lexbuf lexdata)
    } 
 | "<!" ['D' 'd']['O' 'o']['C' 'c']['T' 't']['Y' 'y']['P' 'p']['E' 'e'] 
         [^ '>']* '>'
    { (fun lexdata ->
       [],
       Doctype (Lexing.lexeme lexbuf),
       Loc (Lexing.lexeme_start lexbuf - lexdata.pos_fix, Lexing.lexeme_end lexbuf - lexdata.pos_fix) )}

 | eof 
    { (fun lexdata -> 
       [],
       EOF,
       Loc (Lexing.lexeme_start lexbuf - lexdata.pos_fix, Lexing.lexeme_end lexbuf - lexdata.pos_fix) )}
 | '&' 
    { (fun lexdata ->
     lexdata.start <- Lexing.lexeme_start lexbuf - lexdata.pos_fix;
     Ebuffer.reset lexdata.buffer;
     Ebuffer.output_string lexdata.buffer (ampersand lexbuf lexdata);
     text lexbuf lexdata )}
 | "\r\n" 
    { (fun lexdata ->
     lexdata.start <- Lexing.lexeme_start lexbuf - lexdata.pos_fix;
     Ebuffer.reset lexdata.buffer;
     Ebuffer.output_char lexdata.buffer '\n'; 
     text lexbuf lexdata )}
 | "\r" 
    { (fun lexdata ->
     lexdata.start <- Lexing.lexeme_start lexbuf - lexdata.pos_fix;
     Ebuffer.reset lexdata.buffer;
     Ebuffer.output_char lexdata.buffer '\n'; 
     text lexbuf lexdata )}
 | "\027\040\066" (* ASCII *)
     { (fun lexdata ->
     lexdata.start <- Lexing.lexeme_start lexbuf - lexdata.pos_fix;
     lexdata.pos_fix <- lexdata.pos_fix + 3;
     Ebuffer.reset lexdata.buffer;
     text lexbuf lexdata )}
(*
 | "\027\036" '\040'? ['\064' - '\068'] (* KANJI *)
     {(fun lexdata ->
       let lexeme = Lexing.lexeme lexbuf in
       let len = String.length lexeme in
       lexdata.start <- Lexing.lexeme_start lexbuf - lexdata.pos_fix;
       lexdata.pos_fix <- lexdata.pos_fix + len;
       Ebuffer.reset lexdata.buffer;
       Ebuffer.output_string lexdata.buffer lexeme;
       let c = Lexing.lexeme_char lexbuf (len - 1) in
       if !Lang.japan then kanji lexbuf lexdata c;
       text lexbuf lexdata )}
*)
 | _ { (fun lexdata ->
     lexdata.start <- Lexing.lexeme_start lexbuf - lexdata.pos_fix;
     Ebuffer.reset lexdata.buffer;
     Ebuffer.output_char lexdata.buffer (Lexing.lexeme_char lexbuf 0); 
     text lexbuf lexdata )}

(* call this ONLY if we are not in strict mode *)
and lenient_end_comment = parse
    "-->"
    {(fun lexdata -> 
      [],
      Comment (Ebuffer.get lexdata.buffer),
      Loc(lexdata.start, Lexing.lexeme_end lexbuf - lexdata.pos_fix))}
  | "\027\040\066" (* ASCII *)
      { (fun lexdata ->
	lexdata.pos_fix <- lexdata.pos_fix + 3;
	Ebuffer.output_string lexdata.buffer (Lexing.lexeme lexbuf);
	lenient_end_comment lexbuf lexdata )}
(*
  | "\027\036" '\040'? ['\064' - '\068'] (* KANJI *)
      {(fun lexdata ->
	let lexeme = Lexing.lexeme lexbuf in
	Ebuffer.output_string lexdata.buffer lexeme;
	let len = String.length lexeme in
	lexdata.pos_fix <- lexdata.pos_fix + len; 
 	let c = Lexing.lexeme_char lexbuf (len - 1) in
	if !Lang.japan then kanji lexbuf lexdata c; 
	lenient_end_comment lexbuf lexdata )}
*)
  | _
     {(fun lexdata ->
       Ebuffer.output_char lexdata.buffer (Lexing.lexeme_char lexbuf 0);
       lenient_end_comment lexbuf lexdata )}
  | ""
    { (fun lexdata ->
   raise (Html_Lexing ("unterminated comment", Lexing.lexeme_start lexbuf - lexdata.pos_fix))
    )}

(* we're looking for the end of a comment : skip all characters until next   *)
(*  -- included, and then look for next -- or > *)
and comment = parse
    (* normal case *)
    "--"
    { (fun lexdata -> next_comment lexbuf lexdata)}
  | "\027\040\066" (* ASCII *)
      { (fun lexdata ->
	Ebuffer.output_string lexdata.buffer (Lexing.lexeme lexbuf);
	lexdata.pos_fix <- lexdata.pos_fix + 3;
        (* do nothing except position fix *)
	comment lexbuf lexdata )}
(*
  | "\027\036" '\040'? ['\064' - '\068'] (* KANJI *)
      {(fun lexdata ->
	let lexeme = Lexing.lexeme lexbuf in
	Ebuffer.output_string lexdata.buffer lexeme;
	let len = String.length lexeme in
	lexdata.pos_fix <- lexdata.pos_fix + len; 
	let c = Lexing.lexeme_char lexbuf (len - 1) in
 	if !Lang.japan then kanji lexbuf lexdata c; 
	comment lexbuf lexdata )}
*)
  | _  
    { (fun lexdata ->
       Ebuffer.output_char lexdata.buffer (Lexing.lexeme_char lexbuf 0);
       comment lexbuf lexdata )}
  | ""
    { (fun lexdata ->
   raise (Html_Lexing ("unterminated comment", Lexing.lexeme_start lexbuf - lexdata.pos_fix))
    )}

(* the normal next comment search *)      
and next_comment = parse
    [' ' '\t' '\r' '\n']* "--"
    { (fun lexdata -> comment lexbuf lexdata )}
  | [' ' '\t' '\r' '\n']* '>'
    { (fun lexdata ->
      [],
      Comment (Ebuffer.get lexdata.buffer),
      Loc(lexdata.start, Lexing.lexeme_end lexbuf - lexdata.pos_fix))}
  | "" 
    { (fun lexdata ->
      raise (Html_Lexing ("invalid comment", Lexing.lexeme_start lexbuf - lexdata.pos_fix)))
     }

and text = parse
 | "\027\040\066" (* ASCII *)
     { (fun lexdata ->
     lexdata.start <- Lexing.lexeme_start lexbuf - lexdata.pos_fix;
     lexdata.pos_fix <- lexdata.pos_fix + 3;
     Ebuffer.output_string lexdata.buffer (Lexing.lexeme lexbuf);
     text lexbuf lexdata )}
(*
 | "\027\036" '\040'? ['\064' - '\068'] (* KANJI *)
     {(fun lexdata ->
       let lexeme = Lexing.lexeme lexbuf in
       let len = String.length lexeme in
       lexdata.start <- Lexing.lexeme_start lexbuf - lexdata.pos_fix;
       lexdata.pos_fix <- lexdata.pos_fix + len;
       Ebuffer.output_string lexdata.buffer lexeme;
       let c = Lexing.lexeme_char lexbuf (len - 1) in
       if !Lang.japan then kanji lexbuf lexdata c;
       text lexbuf lexdata )}
*)
 | [^ '<' '&' '\r' '\027']+
    { (fun lexdata ->
      let _sTODO = Lexing.lexeme lexbuf in
      Ebuffer.output_string lexdata.buffer (Lexing.lexeme lexbuf);	
      text lexbuf lexdata )}
 | [^ '<' '&' '\r' '\027']* '&'
    { (fun lexdata ->
       let lexeme = Lexing.lexeme lexbuf in
      	 Ebuffer.output lexdata.buffer lexeme 0 (String.length lexeme -1) ;
	 Ebuffer.output_string lexdata.buffer (ampersand lexbuf lexdata); 
      	 text lexbuf lexdata )}
 | [^ '<' '&' '\r' '\027']* '\r' '\n'
    { (fun lexdata ->
      	let lexeme = Lexing.lexeme lexbuf in
      	 Ebuffer.output lexdata.buffer lexeme 0 (String.length lexeme - 2);
         Ebuffer.output_char lexdata.buffer '\n';
      	 text lexbuf lexdata )}
 | [^ '<' '&' '\r' '\027']* '\r'
    { (fun lexdata ->
        let lexeme = Lexing.lexeme lexbuf in
      	 Ebuffer.output lexdata.buffer lexeme 0 (String.length lexeme - 1);
         Ebuffer.output_char lexdata.buffer '\n';
      	 text lexbuf lexdata )}
 | ""
    { (fun lexdata ->    
      [],
      PCData (Ebuffer.get lexdata.buffer), 
      Loc(lexdata.start, Lexing.lexeme_end lexbuf - lexdata.pos_fix)
      )}
 (* no default case needed *)

and ampersand = parse
   '#' ['0'-'9']+ ';' 
    { (fun lexdata ->
        let lexeme = Lexing.lexeme lexbuf in
      	let code = String.sub lexeme 1 (String.length lexeme - 2) in
	try 
	  (*if !Lang.japan then
	    "\027\040\066" ^ String.make 1 (Char.chr (int_of_string code))
	  else
      *)
	  String.make 1 (Char.chr (int_of_string code))
    with (* #350 ... *) Invalid_argument _ -> " "
      )}
 | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']* ';'
    { (fun lexdata ->
        let lexeme = Lexing.lexeme lexbuf in
      	let entity = String.sub lexeme 0 (String.length lexeme - 1) in
	  begin try 
	    get_entity entity
	  with (* 4.2.1 undeclared markup error handling *)
	    Not_found ->
	      ("&" ^ lexeme)
          end
      )}
  (* terminating ; is not required if next character could not be 
     part of the lexeme *)
 | '#' ['0'-'9']+
    { (fun lexdata ->
        let lexeme = Lexing.lexeme lexbuf in
      	let code = String.sub lexeme 1 (String.length lexeme - 1) in
	try 
	  (*if !Lang.japan then
	    "\027\040\066" ^ String.make 1 (Char.chr (int_of_string code))
	  else
      *)
	  String.make 1 (Char.chr (int_of_string code))
	with Invalid_argument _ -> " "
      )}
 | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
    { (fun lexdata ->
      	let lexeme = Lexing.lexeme lexbuf in
	  try get_entity lexeme
	  with (* 4.2.1 undeclared markup error handling *)
	    Not_found -> ("&"^lexeme)
      )}
  (* Tolerance ... *)
  | ""
    { (fun lexdata -> "&" )}


(* TODO 2.0: 
 *   syntax for SHORTTAG YES (need to know the DTD for this !).
 *
 *)
and opentag = parse
   ['a'-'z' 'A'-'Z' '0'-'9' '.' '-']+
    { (fun lexdata ->
         let tagname = String.lowercase (Lexing.lexeme lexbuf)
         and attribs = ref []
	 and bugs = ref [] in
	 let rec read_attribs () =
	   match attrib lexbuf lexdata with
	     Closetag n -> n
	   | Attribute(p1, p2) ->
      	       attribs := (p1, p2) :: !attribs; read_attribs()
	   | Bogus (reason,pos) ->
	       bugs := (reason,pos) :: !bugs; read_attribs() in
	 let e = read_attribs() in
	  !bugs,
          OpenTag {tag_name = tagname; attributes = List.rev !attribs },
	  Loc(lexdata.start, e)
        )}
  
(* Tolerance *)
  | ""  
    { (fun lexdata ->
      	  Ebuffer.reset lexdata.buffer;
          Ebuffer.output_char lexdata.buffer '<';
          text lexbuf lexdata)}

and closetag = parse
  | ['a'-'z' 'A'-'Z' '0'-'9' '.' '-']+
    { (fun lexdata ->
        let lexeme = Lexing.lexeme lexbuf in
	let e = skip_to_close lexbuf lexdata in
	[],
	CloseTag (String.lowercase lexeme), Loc(lexdata.start,e))}
(* Tolerance *)
  | ""  
    { (fun lexdata ->
      	  Ebuffer.reset lexdata.buffer;
          Ebuffer.output_string lexdata.buffer "</";
          text lexbuf lexdata)}

and attrib = parse
    [' ' '\t' '\n' '\r']+ 
    { (fun lexdata -> attrib lexbuf lexdata )}
  | ['a'-'z' 'A'-'Z' '0'-'9' '.' '-']+ 
    { (fun lexdata ->
      	let name = String.lowercase(Lexing.lexeme lexbuf) in
	try
      	  match tagattrib lexbuf lexdata with
      	    Some s -> Attribute (name, s)
      	  | None -> Attribute (name, name)
	with
	  Html_Lexing(reason,pos) -> 
	    if !strict then raise (Html_Lexing(reason,pos))
	    else Bogus(reason,pos)
      )}
   (* added '_' so we can parse Netscape bookmark files, 
      but it should NOT be there *)
  | ['a'-'z' 'A'-'Z' '0'-'9' '.' '-' '_']+ 
    { (fun lexdata ->
       let name = String.lowercase(Lexing.lexeme lexbuf) in
      	 if !strict then
      	  raise (Html_Lexing ("illegal attribute name: " ^ name,
                                Lexing.lexeme_start lexbuf - lexdata.pos_fix))
	 else
	   try
	     match tagattrib lexbuf lexdata with
	       Some s -> Attribute (name, s)
	     | None -> Attribute (name, name)
	   with
	     Html_Lexing(reason,pos) -> Bogus(reason,pos)
      )}
  | '>' '\n'?
    { (fun lexdata -> Closetag (Lexing.lexeme_end lexbuf - lexdata.pos_fix) )}
  | eof 
    { (fun lexdata -> raise (Html_Lexing ("unclosed tag",
			                   Lexing.lexeme_start lexbuf - lexdata.pos_fix)))}
  (* tolerance: we are expecting an attribute name, but can't get any *)
  (* skip the char and try again. (The char cannot be > !) *)
  | _
    { (fun lexdata ->
        if !strict then
	  raise (Html_Lexing ("invalid attribute name",
			       Lexing.lexeme_start lexbuf - lexdata.pos_fix))
	else Bogus ("invalid attribute name", Lexing.lexeme_start lexbuf - lexdata.pos_fix)
    )}

and tagattrib = parse
    [' ' '\t' '\n' '\r']* '=' [' ' '\t' '\n' '\r']*
    { (fun lexdata -> Some (attribvalue lexbuf lexdata) )}
  | "" 
    { (fun lexdata -> None )}
  
(* This should be dependent on the attribute name *)
(* people often forget to quote, so try to do something about it *)
(* but if a quote is not closed, you are dead *)
and attribvalue = parse
    ['a'-'z' 'A'-'Z' '0'-'9' '.' '-']+ 
    { (fun lexdata -> Lexing.lexeme lexbuf )}
  | '"' 
    { (fun lexdata ->
       Ebuffer.reset lexdata.buffer; inquote lexbuf lexdata )}
  | '\''
    { (fun lexdata ->
       Ebuffer.reset lexdata.buffer; insingle lexbuf lexdata )}
  | ("\027\036" '\040'? ['\064' - '\068'] (* remove control codes *)
        ['\033' - '\126']+ | (* suppose we have no empty KANJI seq *)
     [^ '"' '\'' '>' '\027']) 
    ("\027\036" '\040'? ['\064' - '\068'] ['\033' - '\126']+ |
     "\027\040\066" |
     [^ ' ' '\t' '\n' '\r' '>' '\027']*)
    { (fun lexdata ->
       let lexeme = Lexing.lexeme lexbuf in
       lexdata.pos_fix <- String.length lexeme - Lexkanji.length lexeme;
       lexeme )}
  | "" 
    { (fun lexdata ->
        raise (Html_Lexing ("illegal attribute val",
                              Lexing.lexeme_start lexbuf)) )}

and inquote = parse
    [^ '"' '&' '\027']+
    { (fun lexdata ->
       	 Ebuffer.output_string lexdata.buffer (Lexing.lexeme lexbuf);
      	 inquote lexbuf lexdata )}
  | "\027\040\066" (* ASCII *)
      { (fun lexdata ->
	lexdata.pos_fix <- lexdata.pos_fix + 3;
	Ebuffer.output_string lexdata.buffer (Lexing.lexeme lexbuf);
	inquote lexbuf lexdata )}
(*
  | "\027\036" '\040'? ['\064' - '\068'] (* KANJI *)
      {(fun lexdata ->
	let lexeme = Lexing.lexeme lexbuf in
	Ebuffer.output_string lexdata.buffer lexeme;
	let len = String.length lexeme in
	lexdata.pos_fix <- lexdata.pos_fix + len; 
	let c = Lexing.lexeme_char lexbuf (len - 1) in
 	if !Lang.japan then kanji lexbuf lexdata c;
	inquote lexbuf lexdata )}
*)
  | '"'
    { (fun lexdata ->
      	 beautify true (Ebuffer.get lexdata.buffer) )}
  | '&'
    { (fun lexdata ->
      	Ebuffer.output_string lexdata.buffer (ampersand lexbuf lexdata);
      	inquote lexbuf lexdata )}
  | ""
    { (fun lexdata ->
     raise (Html_Lexing ("unclosed \"", Lexing.lexeme_start lexbuf - lexdata.pos_fix))
     )}
  
and insingle = parse
    [^ '\'' '&']+
    { (fun lexdata ->
      	Ebuffer.output_string lexdata.buffer (Lexing.lexeme lexbuf);
      	insingle lexbuf lexdata )}
  | '\''
    { (fun lexdata -> beautify true (Ebuffer.get lexdata.buffer) )}
  | '&'
    { (fun lexdata ->
      	Ebuffer.output_string lexdata.buffer (ampersand lexbuf lexdata);
      	insingle lexbuf lexdata )}
  | ""
    { (fun lexdata ->
    raise (Html_Lexing ("unclosed '", Lexing.lexeme_start lexbuf - lexdata.pos_fix))
    )}

and skip_to_close = parse
   [^'>']* '>' { (fun lexdata -> Lexing.lexeme_end lexbuf - lexdata.pos_fix)}
  | "" { (fun lexdata -> 
      raise (Html_Lexing ("unterminated tag", 
			  Lexing.lexeme_start lexbuf - lexdata.pos_fix)) )}

and cdata = parse
   [^ '<']* (['<']+ [^ '/']) ? { 
      (fun lexdata ->
        [],
      	CData(Lexing.lexeme lexbuf),
        Loc(Lexing.lexeme_start lexbuf - lexdata.pos_fix, Lexing.lexeme_end lexbuf - lexdata.pos_fix))}
      
  | "</" ['a'-'z' 'A'-'Z' '0'-'9' '.' '-']+
    { (fun lexdata ->
        let lexeme = Lexing.lexeme lexbuf in
	let _eTODO = skip_to_close lexbuf lexdata in
	 [],
	 CloseTag (String.lowercase 
                      (String.sub lexeme 2 (String.length lexeme - 2))),
        Loc(Lexing.lexeme_start lexbuf - lexdata.pos_fix, Lexing.lexeme_end lexbuf - lexdata.pos_fix))}
  | "</" {
      (fun lexdata ->
        [],
      	CData(Lexing.lexeme lexbuf),
        Loc(Lexing.lexeme_start lexbuf - lexdata.pos_fix, Lexing.lexeme_end lexbuf - lexdata.pos_fix)) }

  | eof
    { (fun lexdata ->
        [],
        EOF,
        Loc(Lexing.lexeme_start lexbuf - lexdata.pos_fix, Lexing.lexeme_end lexbuf - lexdata.pos_fix)) }
    
and kanji = parse
  | ['\033' - '\126']+ 
    { (fun lexdata charset ->
      match charset with
	'\064' | '\066' | '\068' (* JISX0208/JISX0212 *) ->
	  let lexeme = Lexing.lexeme lexbuf in
	  let rawlength = String.length lexeme in
	  if rawlength mod 2 = 1 then 
	    Log.f ("Warning: Lexhtml: Odd bytes Kanji string length");
	  let klength = rawlength / 2 in
	  lexdata.pos_fix <- lexdata.pos_fix + klength;
	  Ebuffer.output_string lexdata.buffer lexeme;
	  kanji lexbuf lexdata charset
      |	_ -> raise (Failure (Printf.sprintf "Lexhtml: Unknown charset %d" 
			       (Char.code charset))) 
    )}
  | ['\000' - '\026' '\028' - '\032' '\127'] (* Control codes *) 
    { (fun lexdata charset ->
      Ebuffer.output_char lexdata.buffer (Lexing.lexeme_char lexbuf 0);
      kanji lexbuf lexdata charset )}
  | ['\128' - '\255'] (* Right side *)
    { (fun lexdata charset ->
        raise (Failure "Lexhtml: Unexpected right side character") )}
  | "" (* Must be escape *)
    { (fun lexdata charset -> () )}

(*s: html/lexhtml.mll *)
(* An HTML lexer *)
{
open Html

(*s: type [[Lexhtml.tagtoken]] *)
type tagtoken =
 | Attribute of string * string
 | Closetag of int
 | Bogus of string * int   (* Bogus(s,n) == bug at pos [n] for reason [s] *)
(*e: type [[Lexhtml.tagtoken]] *)

(*s: type [[Lexhtml.t]] *)
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
  (*mutable*) pos_fix : int
}
(*e: type [[Lexhtml.t]] *)

(*s: function [[Lexhtml.new_data]] *)
let new_data () = {
  buffer = Ebuffer.create 512;
  start = 0;
  pos_fix = 0 
}
(*e: function [[Lexhtml.new_data]] *)

(*s: global [[Lexhtml.strict]] *)
let strict = ref false
(*e: global [[Lexhtml.strict]] *)

(*s: type [[Lexhtml.warnings]] *)
type warnings = (string * int) list
(*e: type [[Lexhtml.warnings]] *)

(*s: helper functions [[Lexhtml.xxx]] *)
let noerr = []
(*x: helper functions [[Lexhtml.xxx]] *)
let mk_start lexbuf lexdata =
  Lexing.lexeme_start lexbuf - lexdata.pos_fix
let mk_end lexbuf lexdata =
  Lexing.lexeme_end lexbuf - lexdata.pos_fix
let mk_loc lexbuf lexdata =
  Loc (mk_start lexbuf lexdata, mk_end lexbuf lexdata)
(*e: helper functions [[Lexhtml.xxx]] *)

let numeric_entity_to_utf8 code =
  try
    let n = int_of_string code in
    if n > 0x10FFFF then " " else Html.utf8_of_codepoint n
  with Failure _ -> " "
}

(*s: function [[Lexhtml.html]] *)
rule html = parse
(*s: [[Lexhtml.html()]] rule cases *)
| "<!>" 
    { (fun lexdata ->
       noerr, Comment "", mk_loc lexbuf lexdata)}
(*x: [[Lexhtml.html()]] rule cases *)
(* If you think it is possible to deal with malformed comments adaptatively,
   that is switching to lenient mode only after we detected an error
   in comment syntax, then ponder the following example: <!-- -- --> *)
 | "<!--"
    { (fun lexdata ->
       lexdata.start <- mk_start lexbuf lexdata;
       Ebuffer.reset lexdata.buffer;
       if !strict 
       then comment lexbuf lexdata
       else lenient_end_comment lexbuf lexdata
       )
    } 
(*x: [[Lexhtml.html()]] rule cases *)
| '<' 
    { (fun lexdata ->
       lexdata.start <- mk_start lexbuf lexdata;
       opentag lexbuf lexdata
     )}
(*x: [[Lexhtml.html()]] rule cases *)
| '\n'? "</"
    { (fun lexdata ->
       lexdata.start <- mk_start lexbuf lexdata;
       closetag lexbuf lexdata
      )}
(*x: [[Lexhtml.html()]] rule cases *)
| "<!" ['D''d']['O''o']['C''c']['T''t']['Y''y']['P''p']['E''e'] [^ '>']* '>'
    { (fun lexdata ->
       noerr, Doctype (Lexing.lexeme lexbuf), mk_loc lexbuf lexdata )}
(*x: [[Lexhtml.html()]] rule cases *)
 | '&' 
    { (fun lexdata ->
       lexdata.start <- mk_start lexbuf lexdata;
       Ebuffer.reset lexdata.buffer;
       Ebuffer.output_string lexdata.buffer 
         (ampersand lexbuf lexdata);
       text lexbuf lexdata )}
(*e: [[Lexhtml.html()]] rule cases *)
 | "\r\n" 
    { (fun lexdata ->
       lexdata.start <- mk_start lexbuf lexdata;
       Ebuffer.reset lexdata.buffer;
       Ebuffer.output_char lexdata.buffer '\n'; 
       text lexbuf lexdata )}
 | "\r" 
    { (fun lexdata ->
       lexdata.start <- mk_start lexbuf lexdata;
       Ebuffer.reset lexdata.buffer;
       Ebuffer.output_char lexdata.buffer '\n'; 
       text lexbuf lexdata )}
| eof 
    { (fun lexdata -> 
        (noerr, EOF, mk_loc lexbuf lexdata)) }
| _ { (fun lexdata ->
       lexdata.start <- mk_start lexbuf lexdata;
       Ebuffer.reset lexdata.buffer;
       Ebuffer.output_char lexdata.buffer (Lexing.lexeme_char lexbuf 0); 
       text lexbuf lexdata )}
(*e: function [[Lexhtml.html]] *)


(*s: function [[Lexhtml.lenient_end_comment]] *)
(* call this ONLY if we are not in strict mode *)
and lenient_end_comment = parse
| "-->"
    {(fun lexdata -> 
      noerr, Comment (Ebuffer.get lexdata.buffer),
      Loc(lexdata.start, mk_end lexbuf lexdata))}
| _
    {(fun lexdata ->
      Ebuffer.output_char lexdata.buffer (Lexing.lexeme_char lexbuf 0);
      lenient_end_comment lexbuf lexdata )}
| ""
    {(fun lexdata ->
       raise (Html_Lexing ("unterminated comment", mk_start lexbuf lexdata))
    )}
(*e: function [[Lexhtml.lenient_end_comment]] *)

(*s: function [[Lexhtml.comment]] *)
(* we're looking for the end of a comment : skip all characters until next   *)
(*  -- included, and then look for next -- or > *)
and comment = parse
  (* normal case *)
| "--"
    { (fun lexdata -> 
       next_comment lexbuf lexdata)}
| _  
    { (fun lexdata ->
       Ebuffer.output_char lexdata.buffer (Lexing.lexeme_char lexbuf 0);
       comment lexbuf lexdata )}
| ""
    { (fun lexdata ->
       raise (Html_Lexing ("unterminated comment", mk_start lexbuf lexdata))
    )}
(*e: function [[Lexhtml.comment]] *)

(*s: function [[Lexhtml.next_comment]] *)
(* the normal next comment search *)      
and next_comment = parse
    [' ' '\t' '\r' '\n']* "--"
    { (fun lexdata -> comment lexbuf lexdata )}
  | [' ' '\t' '\r' '\n']* '>'
    { (fun lexdata ->
      [],
      Comment (Ebuffer.get lexdata.buffer),
      Loc(lexdata.start, mk_end lexbuf lexdata))}
  | "" 
    { (fun lexdata ->
      raise (Html_Lexing ("invalid comment", mk_start lexbuf lexdata)))
     }
(*e: function [[Lexhtml.next_comment]] *)


(*s: function [[Lexhtml.text]] *)
and text = parse
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
       noerr, 
       PCData (Ebuffer.get lexdata.buffer), 
       Loc(lexdata.start, mk_end lexbuf lexdata)
      )}
 (* no default case needed *)
(*e: function [[Lexhtml.text]] *)

(*s: function [[Lexhtml.ampersand]] *)
and ampersand = parse
| '#' ['0'-'9']+ ';'
    { (fun _lexdata ->
        let lexeme = Lexing.lexeme lexbuf in
        numeric_entity_to_utf8 (String.sub lexeme 1 (String.length lexeme - 2))
      )}
| ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']* ';'
    { (fun _lexdata ->
        let lexeme = Lexing.lexeme lexbuf in
        let entity = String.sub lexeme 0 (String.length lexeme - 1) in
        try 
          get_entity entity
        with (* 4.2.1 undeclared markup error handling *) Not_found ->
          ("&" ^ lexeme)
      )}
  (* terminating ; is not required if next character could not be 
     part of the lexeme *)
| '#' ['0'-'9']+
    { (fun _lexdata ->
        let lexeme = Lexing.lexeme lexbuf in
        numeric_entity_to_utf8 (String.sub lexeme 1 (String.length lexeme - 1))
      )}
 | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
    { (fun _lexdata ->
        let lexeme = Lexing.lexeme lexbuf in
        try 
          get_entity lexeme
        with (* 4.2.1 undeclared markup error handling *) Not_found -> 
          ("&"^lexeme)
      )}
  (* Tolerance ... *)
| ""
    { (fun _lexdata -> "&" )}
(*e: function [[Lexhtml.ampersand]] *)

(*s: function [[Lexhtml.opentag]] *)
(* TODO 2.0: 
 *   syntax for SHORTTAG YES (need to know the DTD for this !).
 *
 *)
and opentag = parse
| ['a'-'z' 'A'-'Z' '0'-'9' '.' '-']+
    { (fun lexdata ->
       let tagname = String.lowercase_ascii (Lexing.lexeme lexbuf) in
       let attribs = ref [] in
       let bugs = ref [] in
       let rec read_attribs () =
         match attrib lexbuf lexdata with
         | Closetag n -> 
             n
         | Attribute(p1, p2) ->
             attribs := (p1, p2) :: !attribs; 
             read_attribs()
         | Bogus (reason,pos) ->
             bugs := (reason,pos) :: !bugs; 
             read_attribs() 
       in
       let e = read_attribs() in
       (!bugs, 
       OpenTag {tag_name = tagname; attributes = List.rev !attribs },
       Loc(lexdata.start, e)
       )
      )}
  
(* Tolerance *)
| ""  
    { (fun lexdata ->
       Ebuffer.reset lexdata.buffer;
       Ebuffer.output_char lexdata.buffer '<';
       text lexbuf lexdata )}
(*e: function [[Lexhtml.opentag]] *)
(*s: function [[Lexhtml.closetag]] *)
and closetag = parse
  | ['a'-'z' 'A'-'Z' '0'-'9' '.' '-']+
    { (fun lexdata ->
        let lexeme = Lexing.lexeme lexbuf in
    let e = skip_to_close lexbuf lexdata in
    [],
    CloseTag (String.lowercase_ascii lexeme), Loc(lexdata.start,e))}
(* Tolerance *)
  | ""  
    { (fun lexdata ->
          Ebuffer.reset lexdata.buffer;
          Ebuffer.output_string lexdata.buffer "</";
          text lexbuf lexdata)}
(*e: function [[Lexhtml.closetag]] *)

(*s: function [[Lexhtml.attrib]] *)
and attrib = parse
| [' ' '\t' '\n' '\r']+ 
    { (fun lexdata -> attrib lexbuf lexdata )}
| ['a'-'z' 'A'-'Z' '0'-'9' '.' '-']+ 
    { (fun lexdata ->
        let name = String.lowercase_ascii (Lexing.lexeme lexbuf) in
        try
          match tagattrib lexbuf lexdata with
          | Some s -> Attribute (name, s)
          | None -> Attribute (name, name)
        with Html_Lexing(reason,pos) -> 
          if !strict 
          then raise (Html_Lexing(reason,pos));
          Bogus(reason,pos)
      )}
  (* added '_' so we can parse Netscape bookmark files, 
      but it should NOT be there *)
| ['a'-'z' 'A'-'Z' '0'-'9' '.' '-' '_']+ 
    { (fun lexdata ->
       let name = String.lowercase_ascii (Lexing.lexeme lexbuf) in
       if !strict 
       then raise (Html_Lexing ("illegal attribute name: " ^ name,
                                 mk_start lexbuf lexdata))
       else
         try
           match tagattrib lexbuf lexdata with
           | Some s -> Attribute (name, s)
           | None -> Attribute (name, name)
         with Html_Lexing(reason,pos) -> Bogus(reason,pos)
      )}
| '>' '\n'?
    { (fun lexdata -> Closetag (mk_end lexbuf lexdata) )}
| eof 
    { (fun lexdata -> raise (Html_Lexing ("unclosed tag",
                               mk_start lexbuf lexdata)))}

(* tolerance: we are expecting an attribute name, but can't get any *)
(* skip the char and try again. (The char cannot be > !) *)
| _
    { (fun lexdata ->
        if !strict 
        then raise (Html_Lexing ("invalid attribute name",
                                  mk_start lexbuf lexdata));
        Bogus ("invalid attribute name", mk_start lexbuf lexdata)
    )}
(*e: function [[Lexhtml.attrib]] *)
(*s: function [[Lexhtml.tagattrib]] *)
and tagattrib = parse
| [' ' '\t' '\n' '\r']* '=' [' ' '\t' '\n' '\r']*
    { (fun lexdata -> Some (attribvalue lexbuf lexdata) )}
| "" 
    { (fun _lexdata -> None )}
(*e: function [[Lexhtml.tagattrib]] *)
(*s: function [[Lexhtml.attribvalue]] *)
(* This should be dependent on the attribute name *)
(* people often forget to quote, so try to do something about it *)
(* but if a quote is not closed, you are dead *)
and attribvalue = parse
| ['a'-'z' 'A'-'Z' '0'-'9' '.' '-']+ 
    { (fun _lexdata -> Lexing.lexeme lexbuf )}
| '"' 
    { (fun lexdata ->
       Ebuffer.reset lexdata.buffer; 
       inquote lexbuf lexdata )}
| '\''
    { (fun lexdata ->
       Ebuffer.reset lexdata.buffer; 
       insingle lexbuf lexdata )}
| "" 
    { (fun _lexdata ->
        raise (Html_Lexing ("illegal attribute val",
                              Lexing.lexeme_start lexbuf)) )}
(*e: function [[Lexhtml.attribvalue]] *)


(*s: function [[Lexhtml.inquote]] *)
and inquote = parse
| [^ '"' '&' '\027']+
    { (fun lexdata ->
       Ebuffer.output_string lexdata.buffer (Lexing.lexeme lexbuf);
       inquote lexbuf lexdata )}
| '"'
    { (fun lexdata ->
       Html.beautify true (Ebuffer.get lexdata.buffer) )}
| '&'
    { (fun lexdata ->
       Ebuffer.output_string lexdata.buffer (ampersand lexbuf lexdata);
       inquote lexbuf lexdata )}
| ""
    { (fun lexdata ->
       raise (Html_Lexing ("unclosed \"", mk_start lexbuf lexdata))
     )}
(*e: function [[Lexhtml.inquote]] *)

(*s: function [[Lexhtml.insingle]] *)
and insingle = parse
| [^ '\'' '&']+
    { (fun lexdata ->
       Ebuffer.output_string lexdata.buffer (Lexing.lexeme lexbuf);
       insingle lexbuf lexdata )}
| '\''
    { (fun lexdata -> 
       Html.beautify true (Ebuffer.get lexdata.buffer) )}
| '&'
    { (fun lexdata ->
       Ebuffer.output_string lexdata.buffer (ampersand lexbuf lexdata);
       insingle lexbuf lexdata )}
| ""
    { (fun lexdata ->
       raise (Html_Lexing ("unclosed '", mk_start lexbuf lexdata))
    )}
(*e: function [[Lexhtml.insingle]] *)

(*s: function [[Lexhtml.skip_to_close]] *)
and skip_to_close = parse
   [^'>']* '>' { (fun lexdata -> mk_end lexbuf lexdata)}
  | "" { (fun lexdata -> 
      raise (Html_Lexing ("unterminated tag", 
              mk_start lexbuf lexdata)) )}
(*e: function [[Lexhtml.skip_to_close]] *)

(*s: function [[Lexhtml.cdata]] *)
and cdata = parse
| [^ '<']* (['<']+ [^ '/']) ? 
    { (fun lexdata ->
        noerr, CData(Lexing.lexeme lexbuf), mk_loc lexbuf lexdata)}
      
| "</" ['a'-'z' 'A'-'Z' '0'-'9' '.' '-']+
    { (fun lexdata ->
        let lexeme = Lexing.lexeme lexbuf in
        let _eTODO = skip_to_close lexbuf lexdata in
        noerr,
        CloseTag (String.lowercase_ascii 
                      (String.sub lexeme 2 (String.length lexeme - 2))),
        mk_loc lexbuf lexdata)}
| "</" 
    { (fun lexdata ->
        noerr, CData(Lexing.lexeme lexbuf), mk_loc lexbuf lexdata) }

| eof
    { (fun lexdata ->
        noerr, EOF, mk_loc lexbuf lexdata) }
(*e: function [[Lexhtml.cdata]] *)
(*e: html/lexhtml.mll *)

open Lexkanji
open Charset




let debug = ref false

exception OK of int

type code =
    JIS
  | EUC
  | SJIS
  | ISO8859
  | UTF8
  | Binary
  | Unknown

let encode_table = (* I am sure this is not correct *)
  [ "utf[-_]?8", UTF8;
    "iso[-_]?8859[-_]?.*", ISO8859;
    "ascii", ISO8859;
    "iso[-_]?2022[-_]?jp", JIS;
(*
    "\(x[-_]\|\)euc[-_]jp", EUC;
    "\(x[-_]\|\)sjis", SJIS;
*)
    "shift[-_]jis", SJIS;
    "ms[-_]kanji", SJIS ]

type detected_code =
    Code of code
  | Mixed

let detected_code_names = [
  Code JIS, "iso-2022-jp";
  Code EUC, "euc-jp";
  Code SJIS, "sjis";
  Code ISO8859, "iso-8859";
  Code UTF8, "utf-8";
  Code Binary, "binary...";
  Code Unknown, "unknown...";
  Mixed, "mixed coded"
]

type decoder =
    DecoderASCII
  | DecoderJIS
  | DecoderEUCorSJIS
  | DecoderEUC
  | DecoderSJIS
  | DecoderISO8859
  | DecoderUTF8

type read_type = bytes -> int -> int -> int

class read_native (read_func : read_type) =
 object
  val native_read = read_func
  method read = native_read
end

let esc_sequence = function
    ASCII -> "\027\040\066"
  | JISX0201_Roman -> "\027\040\074" 
  | JISX0201_Katakana -> "\027\040\073"
  | JISX0208_1978 -> "\027\036\064"
  | JISX0208_1983 -> "\027\036\066"
  | JISX0212_1990 -> "\027\036\068"
  | UNKNOWN _ | _ -> ""

class read_i18n (read_func) =
 object
  inherit read_native (read_func) (* nothing is inherited ... *)
  method set_code = fun (_code : code) -> ()
  method get_code = Code ISO8859
end

class  virtual read_i18n_implement (read_func) =
 object (self)
  inherit read_i18n (read_func)

  val mutable curcharset = ASCII
  val mutable curcode = Unknown

  val lexbuf = Lexing.from_function (fun buf len -> read_func buf 0 len) 
  val lexdata = Lexkanji.newdata ()
  val outbuf = Wstream.create 100

  method virtual lex_and_store : unit

  method! set_code code =
    curcode <- code

  method! get_code = Code curcode

  method fill_wstream =
    while Wstream.used outbuf = 0 do
      begin 
	try self#lex_and_store with
	  End_of_file ->
	    if Wstream.used lexdata.stored = 0 then raise Exit
      end;
      (* For japanese fucking Hankaku-kanas *) 
      Wstream.output_array outbuf (Jisx0201.convert_katakana (
				   Wstream.get lexdata.stored));
      Wstream.reset lexdata.stored;
    done

  method! read = fun s n1 n2 ->
    (* If it is in Binary mode and there is no tokens in outbuf *)
(*
    if curcode = Binary && Wstream.used outbuf = 0 then begin
      native_read s n1 n2
    end else *)
    begin
      
    let curpos = ref n1 in
    let curlen = ref 0 in

    try while true do
      (* Fill the buffer if it is void *)
      self#fill_wstream;
    
      (* Here the buffer contains some data *)
      let insert_text charset src =
	(* if charset = ASCII and it contains chars above \128,
	 * they should be preceded with esc sequence
	 * to tell Tk that it is JIS encoded 
	 *)  
	let src =
	  if charset = ASCII then
	    let buf = Ebuffer.create (String.length src * 2) in
	    for i = 0 to String.length src - 1 do
	      if src.[i] > '\127' then 
		Ebuffer.output_string buf (esc_sequence charset);
	      Ebuffer.output_char buf src.[i]
	    done;
	    Ebuffer.get buf
	  else src
	in
        (* If charset is changed, put the corresponding ESC sequence *)
	let src = 
	  if curcharset <> charset then esc_sequence charset ^ src
	  else src
	in
	let len = String.length src in
	if !curlen + len > n2 then (* we cannot add this string !!! *)
	  raise (OK !curlen);
	String.blit src 0 s !curpos len;
	curpos := !curpos + len;
	curlen := !curlen + len;
	curcharset <- charset
      in

      let chrst, c = Wstream.hd outbuf in
      (* compress charset *)
      let chrst = match chrst with
	ASCII | JISX0201_Roman -> ASCII
      | JISX0208_1978 | JISX0208_1983 | JISX0208_1990 -> JISX0208_1983
      | JISX0212_1990 -> JISX0208_1983
      | _ -> chrst
      in
      insert_text chrst c;
      Wstream.skip1 outbuf;
      (* And if the buffer is filled completely, return *)
      if !curlen = n2 then raise (OK !curlen)

    done;0 with
      Exit -> !curlen
    | OK x -> x
    | e -> Bug.wow e; raise e
    end
end

class read_junet (read_func) =
 object (_self)
  inherit read_i18n_implement (read_func)
  method lex_and_store = junet lexbuf lexdata
end

let try_as_euc lexdata =
  if Ebuffer.used lexdata.unresolved = 0 then raise End_of_file;
  let lexbuf' = Lexing.from_string (Ebuffer.get lexdata.unresolved) in
  Ebuffer.reset lexdata.unresolved;
  try while true do eucjapan lexbuf' lexdata done
  with
    End_of_file -> ()
  | _ -> raise (Lexkanji_Error "Must be EUC but failed parsing")
  
let try_as_sjis lexdata =
  if Ebuffer.used lexdata.unresolved = 0 then raise End_of_file;
  let lexbuf' = Lexing.from_string (Ebuffer.get lexdata.unresolved) in
  Ebuffer.reset lexdata.unresolved;
  try while true do sjis lexbuf' lexdata done
  with
    End_of_file -> ()
  | _ -> raise (Lexkanji_Error "Must be SJIS but failed parsing")

type japanese_config = {
    mutable decoders : decoder list option;
  }
 
let default_config () = {
  decoders = None;
} 

let change_to_jis c = 
  c.decoders <- Some [DecoderASCII; DecoderJIS; DecoderISO8859]
let change_to_sjis c = 
  c.decoders <- Some [DecoderASCII; DecoderSJIS]
let change_to_euc c = 
  c.decoders <- Some [DecoderASCII; DecoderEUC]
let change_to_iso8859 c =
  c.decoders <- Some [DecoderISO8859]
let change_to_utf8 c =
  c.decoders <- Some [DecoderUTF8]
let change_to_autodetect c =
  c.decoders <- None

class read_japanese (read_func, config) =
 object (self)
  inherit read_i18n_implement (read_func)

  val mutable wholecode = Code Unknown

  method! get_code = wholecode

  (* val config = config *) 

  method set_wholecode =
    (* ISO8859 and UTF8 are "weak" *)
    match wholecode with
      Code Unknown | Code ISO8859 | Code UTF8 -> wholecode <- Code curcode;
    | Code code ->
	if curcode <> ISO8859 && curcode <> UTF8 && curcode <> code then wholecode <- Mixed
    | Mixed -> ()

  method unknown_junet lexbuf lexdata =
    junet lexbuf lexdata;
    if !debug then Log.f "[JIS]";
    self#set_code JIS;
    self#set_wholecode

  method unknown_eucjapan lexbuf lexdata =
    eucjapan lexbuf lexdata;
    if !debug then Log.f "[EUC]";
    self#set_code EUC;
    self#set_wholecode

  method unknown_sjis lexbuf lexdata =
    sjis lexbuf lexdata;
    if !debug then Log.f "[SJIS]";
    self#set_code SJIS;
    self#set_wholecode

  method unknown_iso8859 lexbuf lexdata =
    iso8859 lexbuf lexdata;
    if !debug then Log.f "[ISO8859]";
    self#set_code ISO8859;
    self#set_wholecode

  method unknown_utf8 lexbuf lexdata =
    iso8859 lexbuf lexdata;
    if !debug then Log.f "[UTF8]";
    self#set_code UTF8;
    self#set_wholecode

  method unknown_eucorsjis lexbuf lexdata =
    try
      eucorsjis lexbuf lexdata;
      Log.f "eucorsjis must raise exception..."
    with
      (Failure _ as e) ->
	(* the unresolved buffer may be empty due to the error. *)
	begin try try_as_euc lexdata with End_of_file -> () end;
	raise e
    | MustbeEUC -> 
	if !debug then Log.f "EUC/SJIS -> [EUC]";
	try_as_euc lexdata;
	self#set_code EUC;
	self#set_wholecode
    | MustbeSJIS ->
	if !debug then Log.f "EUC/SJIS -> [SJIS]";
	try_as_sjis lexdata;
	self#set_code SJIS;
	self#set_wholecode
    | End_of_file ->
	if !debug then 
	  Log.f "I could not determin the code so suppose EUC";
	try_as_euc lexdata;
	self#set_code EUC;
	self#set_wholecode

  method unknown lexbuf lexdata =
    let get_decoder = function
	DecoderASCII -> ascii
      | DecoderJIS -> self#unknown_junet
      | DecoderEUCorSJIS -> self#unknown_eucorsjis
      | DecoderEUC -> self#unknown_eucjapan
      | DecoderSJIS -> self#unknown_sjis
      | DecoderISO8859 -> self#unknown_iso8859
      | DecoderUTF8 -> self#unknown_utf8
    in
    let default_config = [DecoderASCII; DecoderJIS; DecoderEUCorSJIS] in
      
    let decoders = 
      List.map get_decoder (match config.decoders with
        Some ds -> ds 
      | None -> default_config)
    in
    try 
      List.iter (fun decoder ->
	try 
	  decoder lexbuf lexdata; raise Exit
	with
	  Failure _ -> () (* try the next parser *)
	| Exit -> raise Exit
	| End_of_file -> raise End_of_file
	| e -> Log.f ("Strange error: " ^ Printexc.to_string e)) decoders;
      raise (Failure "No applicable decoders")
    with
      Exit -> ()

    val mutable maxerror = 10
    val mutable error = 0

  method robust decoder lexbuf lexdata =
    try decoder lexbuf lexdata with Failure _s -> 
      self#set_code Unknown
	  
  method robust_error decoder lexbuf lexdata =
      try 
	decoder lexbuf lexdata ;
      with 
	Failure s ->
	  error <- error + 1;
	  Log.f (Printf.sprintf "Jp unrecoverable error (%d) %s" error s);
	  if maxerror >= 0 && error > maxerror then begin
	    if !debug then Log.f "Turns to Binary mode";
	    self#set_code Binary;
	    self#set_wholecode
	  end;
	  remove_garbage lexbuf lexdata
    
  method lex_and_store =
    match curcode with
      Unknown -> self#robust_error self#unknown lexbuf lexdata
    | JIS -> self#robust junet lexbuf lexdata
    | EUC -> self#robust eucjapan lexbuf lexdata
    | SJIS -> self#robust sjis lexbuf lexdata
    | ISO8859 -> self#robust iso8859 lexbuf lexdata
    | UTF8 -> self#robust iso8859 lexbuf lexdata
    | Binary -> remove_garbage lexbuf lexdata (* read 1 char at least *)
end

let input_from_string s = 
  let cur = ref 0 in
  fun buf start len ->
    let r = 
      try
	String.blit s !cur buf start len;
	len
      with
	Invalid_argument _ ->
	  let r = String.length s - !cur in
	    String.blit s !cur buf start r; r
    in
    cur := !cur + r;
    r

let create_read_native read_func = new read_i18n read_func
let create_read_junet read_func = (new read_junet read_func :> read_i18n)
let create_read_japanese read_func config = 
  (new read_japanese (read_func, config) :> read_i18n)

(* receives JIS string and encode it into the corresponding code *)
let encoder code =
  (fun s ->
    match code with
      Mixed -> 
	Log.f "Submitting forms with mixed encodings. Using JIS"; 
	s
    | Code Unknown | Code ISO8859 | Code UTF8 | Code JIS -> s
    | Code code ->
	let wstr = 
	  let lexbuf = Lexing.from_string s in
	  let lexdata = Lexkanji.newdata () in
	  try while true do
	    Lexkanji.junet lexbuf lexdata
	  done; lexdata.stored
	  with
	    End_of_file -> lexdata.stored
	in
	let wstr = Wstream.get wstr in
	begin match code with
	  EUC -> Encode.eucjapan wstr
	| SJIS -> Encode.sjis wstr
	| _ -> Encode.junet wstr (* never happens *)
	end)

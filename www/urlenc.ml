(*s: ./www/urlenc.ml *)
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

open Mstring

(*s: function Urlenc.hexchar *)
let hexchar c = 
  let s = Bytes.make 3 '%'
  and i = Char.code c in
  Bytes.set s 1 (dec_to_hex (i/16));
  Bytes.set s 2 (dec_to_hex (i mod 16));
  Bytes.to_string s
(*e: function Urlenc.hexchar *)

(*s: function Urlenc.decode *)
(* Decode escaped characters *)
(* Note: beware of order of splitting wrt '&' and decoding *)
let decode s =
  let l = String.length s in
  let target = Ebuffer.create l in
  let pos = ref 0 in
  while !pos < l do
    if s.[!pos] = '%' && !pos + 2 < l  then begin
       let c = 16 * hex_to_dec s.[!pos+1] + hex_to_dec s.[!pos+2] in
       Ebuffer.output_char target (Char.chr c);
    pos := !pos + 3
    end else if s.[!pos] = '+' then begin
      Ebuffer.output_char target ' ';
      incr pos
    end else begin
      Ebuffer.output_char target s.[!pos];
      incr pos
      end
  done;
  Ebuffer.get target
(*e: function Urlenc.decode *)

(*s: constant Urlenc.keep_quoted *)
(* Unquote an url path:
   We decode all % except those corresponding to significative
   characters for parsing: /, ?, #, sp, :
 *)
let keep_quoted = 
  ['/'; '?'; '#'; ' '; '\t'; '\r'; '\n'; ':'; '%'; '&'; '='; '+']
(*e: constant Urlenc.keep_quoted *)
(*s: function Urlenc.unquote *)
let unquote s =
  try
    (* optim *)
    let _ = String.index s '%' in
    let l = String.length s in
    let target = Ebuffer.create l in
    let pos = ref 0 in
    (try
      while !pos < l do
       let perpos = String.index_from s !pos '%' in
       if perpos > !pos 
       then Ebuffer.output target s !pos (perpos - !pos);
       pos := perpos;
       if s.[!pos] = '%' && !pos + 2 < l  
       then begin
         let c = 16 * hex_to_dec s.[!pos+1] + hex_to_dec s.[!pos+2] in
         let substc = Char.chr c in
         if List.mem substc keep_quoted 
         then
           for _i = 0 to 2 do
             Ebuffer.output_char target s.[!pos];
             incr pos
           done
         else begin
             Ebuffer.output_char target (Char.chr c);
             pos := !pos + 3
         end
       end else begin
        Ebuffer.output_char target s.[!pos];
        incr pos
       end
     done;
     Ebuffer.get target
   with Not_found -> (* no more substitutions *)
     Ebuffer.output target s !pos (l - !pos);
     Ebuffer.get target
   )
  with Not_found -> s
(*e: function Urlenc.unquote *)

(*s: function Urlenc.encode *)
let encode s =
  let target = Ebuffer.create (String.length s) in
  for pos = 0 to String.length s - 1 do
    match s.[pos] with
      ' ' -> Ebuffer.output_char target '+'
    | '0'..'9' | 'a'..'z' | 'A'..'Z' as c -> Ebuffer.output_char target c
    | '\n' -> Ebuffer.output_string target "%0D%0A"
    | c -> Ebuffer.output_string target (hexchar c)
    done;
  Ebuffer.get target
(*e: function Urlenc.encode *)


(*s: constant Urlenc.strict_form_standard *)
let strict_form_standard = ref true
(*e: constant Urlenc.strict_form_standard *)

(*s: function Urlenc.form_encode *)
let form_encode = function 
 | [] -> ""
 | (e,v)::l ->
    let b = Ebuffer.create 512 in
    Ebuffer.reset b;
    Ebuffer.output_string b (encode e);
    Ebuffer.output_char b '=';
    Ebuffer.output_string b (encode v);
    l |> List.iter (fun (e,v) ->
         Ebuffer.output_char b '&';
         Ebuffer.output_string b 
            (if !strict_form_standard 
             then encode e
             else e
             );
         Ebuffer.output_char b '=';
         Ebuffer.output_string b (encode v)
    ) ;
    Ebuffer.get b
(*e: function Urlenc.form_encode *)

(*s: constant Urlenc.form_decode *)
let form_decode =
  let ampersand c = c = '&' and equals c = c = '=' in
  (function  s ->
     List.map (fun encp ->
       match split_str equals encp with
       [x;y] -> (decode x, decode y)
     | [x] -> (decode x, "")
     | _ -> invalid_arg "form_decode")
       (split_str ampersand s))
(*e: constant Urlenc.form_decode *)
         
(*e: ./www/urlenc.ml *)

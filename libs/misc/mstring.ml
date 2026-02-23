(*s: commons/mstring.ml *)
(*
 * String utilities
 *)

(*s: function [[Mstring.split_str]] *)
(* split a string according to char_sep predicate *)
let split_str char_sep str =
  let len = String.length str in
  if len = 0 then [] else
    let rec skip_sep cur =
      if cur >= len then cur
      else if char_sep str.[cur] then skip_sep (succ cur)
      else cur  in
    let rec split beg cur =
      if cur >= len then 
    if beg = cur then []
    else [String.sub str beg (len - beg)]
      else if char_sep str.[cur] 
       then 
         let nextw = skip_sep cur in
          (String.sub str beg (cur - beg))
        ::(split nextw nextw)
       else split beg (succ cur) in
    let wstart = skip_sep 0 in
    split wstart wstart
(*e: function [[Mstring.split_str]] *)

(*s: function [[Mstring.get_suffix]] *)
(* extract the . suffix (dot excluded) of a string *)
let get_suffix s =
  try
    let dotpos = succ (String.rindex s '.') in
      String.sub s dotpos (String.length s - dotpos)
  with
    Not_found -> ""
(*e: function [[Mstring.get_suffix]] *)

(*s: function [[Mstring.hex_to_dec]] *)
(* HEX/DEC conversions *)
let hex_to_dec c = match c with
    '0'..'9' -> Char.code c - 48
  | 'a'..'f' -> Char.code c - 87 (* 87 = Char.code 'a' - 10 *)
  | 'A'..'F' -> Char.code c - 55 (* 55 = Char.code 'A' - 10 *)
  | _ -> failwith "hex_to_dec"
(*e: function [[Mstring.hex_to_dec]] *)
(*s: function [[Mstring.dec_to_hex]] *)
let dec_to_hex i =
  if i < 10 then Char.chr (i + 48)  (* 48 = Char.code '0' *)
  else Char.chr (i + 55)            (* 55 = Char.code 'A' - 10 *)
(*e: function [[Mstring.dec_to_hex]] *)
(*s: function [[Mstring.hex_to_string]] *)
(* Converting a hex stored string *)
let hex_to_string s =
  let len = String.length s / 2 in
  let res = Bytes.create len in
    for i = 0 to len - 1 do
      Bytes.set res i (Char.chr ( 16 * (hex_to_dec s.[i+i]) + hex_to_dec s.[i+i+1]));
      done;
    Bytes.to_string res
(*e: function [[Mstring.hex_to_string]] *)

(*s: constant [[Mstring.gensym]] *)
let gensym =
  let cnter = ref 0 in
  (fun n ->
    incr cnter;
    n ^ string_of_int !cnter)
(*e: constant [[Mstring.gensym]] *)
(*s: function [[Mstring.egensym]] *)
let egensym s =
  let cnter = ref 0 in
  (fun () ->
    incr cnter;
    s ^ string_of_int !cnter)
(*e: function [[Mstring.egensym]] *)

(*s: function [[Mstring.rem_trailing_sp]] *)
let rem_trailing_sp s =
  let l = String.length s in
  let pos = ref (l - 1) in
  while !pos >= 0 && List.mem s.[!pos] [' '; '\t'] do decr pos done;
  if !pos = l - 1 then s
  else String.sub s 0 (succ !pos)
(*e: function [[Mstring.rem_trailing_sp]] *)


(* Count Unicode codepoints in a UTF-8 encoded string.
 * Unlike String.length (bytes) this matches Tk's CharOffset counting. *)
let utf8_length s =
  let n = String.length s in
  let count = ref 0 in
  let i = ref 0 in
  while !i < n do
    let c = Char.code (String.unsafe_get s !i) in
    if c land 0x80 = 0 then (incr count; incr i)
    else if c land 0xE0 = 0xC0 then (incr count; i := !i + 2)
    else if c land 0xF0 = 0xE0 then (incr count; i := !i + 3)
    else if c land 0xF8 = 0xF0 then (incr count; i := !i + 4)
    else incr i (* continuation byte: skip *)
  done;
  !count


(*s: function [[Mstring.catenate_sep]] *)
let catenate_sep sep =
  function 
      [] -> ""
    | x::l -> List.fold_left (fun s s' -> s^sep^s') x l
(*e: function [[Mstring.catenate_sep]] *)

(*s: function [[Mstring.norm_crlf]] *)
(* Filters CRLF:
 *  CR -> LF
 *  CRLF -> LF
 *  LF -> LF
 * We do this on successive chunks of a stream, so we need to consider
 * the case when the chunk finishes on CR.
 * Assume len > 0
 *)

let norm_crlf lastwascr buf offs len =
  let rpos = ref offs
  and wpos = ref 0
  and dest = Bytes.create (len + 1) (* we need one more char *)
  and limit = offs + len - 1  
  and lastiscr = ref false in
  if lastwascr then
    if buf.[!rpos] = '\n' then begin
      Bytes.set dest !wpos '\n';
      incr rpos; incr wpos
    end
    else begin
      Bytes.set dest !wpos '\n'; incr wpos
    end;

  while !rpos < limit do
    match buf.[!rpos] with
      '\n' -> Bytes.set dest !wpos '\n'; incr rpos; incr wpos
    | '\r' -> 
    if buf.[!rpos + 1] = '\n'
    then begin Bytes.set dest !wpos '\n'; rpos := !rpos + 2; incr wpos end
    else begin Bytes.set dest !wpos '\n'; incr rpos; incr wpos end
    | c -> Bytes.set dest !wpos c; incr rpos; incr wpos 
  done;
  begin match buf.[offs+len-1] with
    '\n' -> Bytes.set dest !wpos '\n'; incr wpos
  | '\r' -> lastiscr := true
  | c -> Bytes.set dest !wpos c; incr wpos
  end;
  Bytes.sub_string dest 0 !wpos, !lastiscr
(*e: function [[Mstring.norm_crlf]] *)
(*e: commons/mstring.ml *)

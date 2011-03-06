open Charset
open Wchar
open Tool

(* This code is to convert JISX0201_Katakana characters into JISX0208 and *)
(* JISX0201_Roman. *)

(* This file must be in Japanese EUC code, becuase some J-EUC coded data *)
(* is written here directly. *) 

let katakana_table =  
  Array.of_list (wcharlist_of_euc "。「」、・ヲァィゥェォャュョッーアイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワン゛゜")
  
let dakuten_table = 
  let wsfrom = wcharlist_of_euc "かきくけこさしすせそたちつてとはひふへほカキクケコサシスセソタチツテトハヒフヘホウ" in
  let wsto =   wcharlist_of_euc "がぎぐげござじずぜぞだぢづでどばびぶべぼガギグゲゴザジズゼゾダヂヅデドバビブベボヴ" in
  List.map2 (fun f t -> f,t) wsfrom wsto

let handakuten_table =
  let wsfrom = wcharlist_of_euc "はひふへほハヒフヘホ" in
  let wsto   = wcharlist_of_euc "ぱぴぷぺぽパピプペポ" in
  List.map2 (fun f t -> f,t) wsfrom wsto
 
let dakuten = match wcharlist_of_euc "゛" with [x] -> x | _ -> assert false
let handakuten = match wcharlist_of_euc "゜" with [x] -> x | _ -> assert false
let s_katakana_table code = 
  if code < Array.length katakana_table && code >= 0 then katakana_table.(code)
  else ASCII, "."
(*  try katakana_table.(code) with e -> ASCII, "." *)

let convert_katakana ws =
  let l = Array.length ws in
  let buf = Wstream.create l in
  let pos = ref 0 in
  if l = 0 then Wstream.get buf else
  try while true do
    if !pos = l - 1 then begin (* last one *)
      begin match ws.(!pos) with
	JISX0201_Katakana, s ->
	  Wstream.output_one buf (s_katakana_table (Char.code s.[0] - 0x21))
      |	_ -> Wstream.output_one buf ws.(!pos)
      end;
      raise Exit
    end else begin
      begin match ws.(!pos), ws.(!pos+1) with
	(JISX0201_Katakana, s), (JISX0201_Katakana, s') -> 
	  let c = s_katakana_table (Char.code s.[0] - 0x21)
	  and c' = s_katakana_table (Char.code s'.[0] - 0x21)
	  in
	  if c' = dakuten then
	    try
	      Wstream.output_one buf (List.assoc c dakuten_table);
	      incr pos
	    with
	      Not_found ->
		Wstream.output_one buf c
	  else if c' = handakuten then
	    try
	      Wstream.output_one buf (List.assoc c handakuten_table);
	      incr pos
	    with
	      Not_found ->
		Wstream.output_one buf c
	  else
	    Wstream.output_one buf c
      | (JISX0201_Katakana, s), _ -> 
	  Wstream.output_one buf (s_katakana_table (Char.code s.[0] - 0x21))
      |	_ -> 
	  Wstream.output_one buf ws.(!pos)
      end;
      incr pos;
      if !pos = l then raise Exit
    end
  done; [| |] with
    Exit -> Wstream.get buf
  | Failure s -> raise (Failure ("convert_katakana: " ^ s)) 
  | _ -> raise (Failure "convert_katakana")


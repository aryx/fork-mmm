open Wchar
open Charset
open Lexkanji

let junet ws =
  let buf = Ebuffer.create (Array.length ws) in
  let curchrst = ref ASCII in
  Array.iter (fun (chrst, s) ->
    if !curchrst <> chrst then Ebuffer.output_string buf (match chrst with
  	ASCII -> "\027(B"
      | JISX0201_Roman -> "\027(J"
      | JISX0201_Katakana -> "\027(I"
      | JISX0208_1978 -> "\027$@"
      | JISX0208_1983 -> "\027$B"
      | JISX0208_1990 -> "\027$B" (* I don't know the ESC SEQ for this *)
      | JISX0212_1990 -> "\027$(D"
      | _ -> "");
    curchrst := chrst;
    Ebuffer.output_string buf s) ws;
  Ebuffer.get buf

let eucjapan ws =
  let buf = Ebuffer.create (Array.length ws) in
  Array.iter (fun (chrst, s) ->
    Ebuffer.output_string buf (match chrst with
      ASCII -> s
    | JISX0201_Roman -> s
    | JISX0201_Katakana -> 
	begin try
          let b = String.create 2 in
	  b.[0] <- '\142';
	  b.[1] <- Char.chr (Char.code s.[0] + 128);
	 b
	with _ -> "." end
    | JISX0208_1978 | JISX0208_1983 | JISX0208_1990 ->
	begin try
          let b = String.create 2 in
	  b.[0] <- Char.chr (Char.code s.[0] + 128);
	  b.[1] <- Char.chr (Char.code s.[1] + 128);
	  b
	with _ -> "." end
    | JISX0212_1990 ->  
	begin try 
	  let b = String.create 3 in
	  b.[0] <- '\143';
	  b.[1] <- Char.chr (Char.code s.[0] + 128);
	  b.[2] <- Char.chr (Char.code s.[1] + 128);
	  b
	with _ -> "." end
    | _ -> s)) ws;
  Ebuffer.get buf

let sjis ws =
  let buf = Ebuffer.create (Array.length ws) in
  Array.iter (fun (chrst, s) ->
    Ebuffer.output_string buf (match chrst with
      ASCII -> s
    | JISX0201_Roman -> s
    | JISX0201_Katakana -> String.make 1 (Char.chr (Char.code s.[0] + 128))
    | JISX0208_1978 | JISX0208_1983 | JISX0208_1990 ->
	let ku = Char.code s.[0] - 32
	and ten = Char.code s.[1] - 32
	in
	let s = String.create 2 in
	if ku <= 62 then begin
	  if ku mod 2 = 1 then begin
	    s.[0] <- Char.chr ((ku + 257) / 2);
	    s.[1] <- Char.chr (if ten <= 63 then ten + 63 else ten + 64)
	  end else begin
	    s.[0] <- Char.chr ((ku + 256) / 2);
	    s.[1] <- Char.chr (ten + 158)
	  end
	end else begin
	  if ku mod 2 = 1 then begin
	    s.[0] <- Char.chr ((ku + 385) / 2);
	    s.[1] <- Char.chr (if ten <= 63 then ten + 63 else ten + 64)
	  end else begin
	    s.[0] <- Char.chr ((ku + 384) / 2);
	    s.[1] <- Char.chr (ten + 158)
	  end
	end;
	s
    | JISX0212_1990 -> (* Heh *) "[JISX0212_1990]" 
    | _ -> s)) ws;
  Ebuffer.get buf

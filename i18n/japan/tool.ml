open Charset
open Wchar

(* Non perfect version of convert function between J-EUC to JISX0208. *)
(* This function is for the library itself only. *)

let wcharlist_of_euc euc = 
  let pos = ref 0 in
  let len = String.length euc in
  let wcharlist = ref [] in
  try while true do
    if !pos >= len then raise (Invalid_argument "Endofstring");
    let wc = 
      match Char.code (euc.[!pos]) with
	c when c < 0x80 -> (ASCII, String.make 1 euc.[!pos])
      | c -> 
	  incr pos;
	  if !pos >= len then raise (Failure "1byte upper char");
	  let c2 = Char.code (euc.[!pos]) in
	  let buf = String.create 2 in
	  buf.[0] <- Char.chr (c - 128);
	  buf.[1] <- Char.chr (c2 - 128);
	  (JISX0208_1983, buf)
    in
    wcharlist := wc :: !wcharlist;
    incr pos;
    if !pos >= len then raise (Invalid_argument "End of string")
  done; raise (Failure "Jisx0201.wcharlist_of_euc")
  with Invalid_argument _ -> List.rev !wcharlist

let euc_of_wchar (charset, s) =
  match charset with
    JISX0208_1978 | JISX0208_1983 | JISX0208_1990 ->
      let s = String.create 2 in
      s.[0] <- Char.chr (Char.code s.[0] + 128);
      s.[1] <- Char.chr (Char.code s.[1] + 128);
      s
  | _ -> s

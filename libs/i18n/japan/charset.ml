type charset =
    ASCII
  | JISX0201_Roman (* JISC6220 *)
  | JISX0201_Katakana
  | JISX0208_1978 (* JISC6226 *)
  | JISX0208_1983
  | JISX0208_1990
  | JISX0212_1990 (* Hojo *)
  | UNKNOWN of string

let length = function
    ASCII | JISX0201_Roman | JISX0201_Katakana -> 1
  | JISX0208_1978 | JISX0208_1983 | JISX0208_1990 | JISX0212_1990 -> 2
  | UNKNOWN _ -> raise Not_found

let singlebyte_terminator_table = [
  '\066', ASCII;
  '\074', JISX0201_Roman;
  '\073', JISX0201_Katakana
] 

let multibyte_terminator_table = [
  '\064', JISX0208_1978;
  '\066', JISX0208_1983;
  '\068', JISX0212_1990
] 


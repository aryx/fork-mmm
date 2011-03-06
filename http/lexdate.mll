{

open Mstring
open Http_date

exception Invalid_date of string * int

}

rule wkday = parse
   "Sun" {0}
 | "Mon" {1}
 | "Tue" {2}
 | "Wed" {3}
 | "Thu" {4}
 | "Fri" {5}
 | "Sat" {6}
 | "" {raise (Invalid_date ("invalid wkday", Lexing.lexeme_start lexbuf))}

and weekday = parse
   "Sunday" {0}
 | "Monday" {1}
 | "Tuesday" {2}
 | "Wednesday" {3}
 | "Thursday" {4}
 | "Friday" {5}
 | "Saturday" {6}
 | "" {raise (Invalid_date ("invalid weekday", Lexing.lexeme_start lexbuf))}

and month = parse
   "Jan" {0}
 | "Feb" {1}
 | "Mar" {2}
 | "Apr" {3}
 | "May" {4}
 | "Jun" {5}
 | "Jul" {6}
 | "Aug" {7}
 | "Sep" {8}
 | "Oct" {9}
 | "Nov" {10}
 | "Dec" {11}
 | "" {raise (Invalid_date ("invalid month", Lexing.lexeme_start lexbuf))}

and twodigit = parse
  ['0'-'9']['0'-'9'] { int_of_string (Lexing.lexeme lexbuf)}
 | "" {raise (Invalid_date ("invalid 2DIGIT", Lexing.lexeme_start lexbuf))}

and fourdigit = parse
  ['0'-'9']['0'-'9']['0'-'9']['0'-'9'] 
      { int_of_string (Lexing.lexeme lexbuf)}
 | "" {raise (Invalid_date ("invalid 4DIGIT", Lexing.lexeme_start lexbuf))}

and wdigit = parse
  ' ' ['0'-'9'] 
    { let s = Lexing.lexeme lexbuf in Char.code s.[1] - Char.code '0' }
 | ['0'-'9']['0'-'9'] 
    { int_of_string (Lexing.lexeme lexbuf)}
 | "" {raise (Invalid_date ("invalid 2DIGIT", Lexing.lexeme_start lexbuf))}

and comma = parse
   ',' { () }
 | "" {raise (Invalid_date ("comma expected", Lexing.lexeme_start lexbuf))}

and semicolon = parse
  ';' { () }
 | "" {raise (Invalid_date ("semicolon expected", Lexing.lexeme_start lexbuf))}

and colon = parse
  ':' { () }
 | "" {raise (Invalid_date ("semicolon expected", Lexing.lexeme_start lexbuf))}

and sp = parse
   ' ' { () }
 | "" {raise (Invalid_date ("SP expected", Lexing.lexeme_start lexbuf))}

and hyphen = parse
   '-' { () }
 | "" {raise (Invalid_date ("SP expected", Lexing.lexeme_start lexbuf))}

and gmt = parse
   "GMT" { () }
 | "" {raise (Invalid_date ("GMT expected", Lexing.lexeme_start lexbuf))}

{

let parse_time lexbuf =
  let hour = twodigit lexbuf in
  let _ = colon lexbuf in
  let min = twodigit lexbuf in
  let _ = colon lexbuf in
  let sec = twodigit lexbuf in
    (hour, min, sec)

let rfc1123 lexbuf =
  let wday = wkday lexbuf in
  let _ = comma lexbuf in
  let _ = sp lexbuf in
  (* date1 *)
  let mday = twodigit lexbuf in
  let _ = sp lexbuf in
  let mon = month lexbuf in
  let _ = sp lexbuf in
  let year = fourdigit lexbuf - 1900 in
  let _ = sp lexbuf in
  let hour,min,sec = parse_time lexbuf in
  let _ = sp lexbuf in
  let _ = gmt lexbuf in
    {ht_sec = sec;
     ht_min = min;
     ht_hour = hour;
     ht_mday = mday;
     ht_mon = mon;
     ht_year = year;
     ht_wday = wday }


let rfc850 lexbuf =
  let wday = weekday lexbuf in
  let _ = comma lexbuf in
  let _ = sp lexbuf in
  (* date2 *)
  let mday = twodigit lexbuf in
  let _ = hyphen lexbuf in
  let mon = month lexbuf in
  let _ = hyphen lexbuf in
  let year = twodigit lexbuf in
  let _ = sp lexbuf in
  let hour,min,sec = parse_time lexbuf in
  let _ = sp lexbuf in
  let _ = gmt lexbuf in
    {ht_sec = sec;
     ht_min = min;
     ht_hour = hour;
     ht_mday = mday;
     ht_mon = mon;
     ht_year = year;
     ht_wday = wday }

let asctime lexbuf =
  let wday = wkday lexbuf in
  let _ = sp lexbuf in
  (* date3 *)
  let mon = month lexbuf in
  let _ = sp lexbuf in
  let mday = wdigit lexbuf in
  let _ = sp lexbuf in
  let hour,min,sec = parse_time lexbuf in
  let _ = sp lexbuf in
  let year = fourdigit lexbuf - 1900 in
    {ht_sec = sec;
     ht_min = min;
     ht_hour = hour;
     ht_mday = mday;
     ht_mon = mon;
     ht_year = year;
     ht_wday = wday }

(* RFC850 has -
 * RFC850 and 822 have commas
 * otherwise it's asctime
 *)

let ht_of_string s =
  try
    let _ = String.index s '-'in
      rfc850 (Lexing.from_string s)
  with
    Not_found ->
      try
        let _ = String.index s ',' in
          rfc1123 (Lexing.from_string s)
      with
      	Not_found -> 
          asctime (Lexing.from_string s)



}

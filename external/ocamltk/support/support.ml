(* Extensible buffers *)
type extensible_buffer = {
    mutable buffer : string;
    mutable pos : int;
    mutable len : int}

let new_buffer () = {
   buffer = String.create 128;
   pos = 0;
   len = 128
   }

let print_in_buffer buf s =
  let l = String.length s in
  if buf.pos + l > buf.len then begin
    buf.buffer <- buf.buffer ^ (String.create (l+128));
    buf.len <- buf.len + 128 + l
    end;
  String.blit s 0 buf.buffer buf.pos l;
  buf.pos <- buf.pos + l

let get_buffer buf = 
  String.sub buf.buffer 0 buf.pos



(* Used by list converters *)
let catenate_sep sep =
  function 
    [] -> ""
  | [x] -> x
  | x::l -> 
      let b = new_buffer() in
      	print_in_buffer b x;
	List.iter (function s -> print_in_buffer b sep; print_in_buffer b s) l;
      get_buffer b

(* Parsing results of Tcl *)
(* List.split a string according to char_sep predicate *)
let split_str char_sep str =
  let len = String.length str in
  let rec skip_sep cur =
    if cur >= len then cur
    else if char_sep (String.get str cur) then skip_sep (succ cur)
    else cur  in
  let rec split beg cur =
    if cur >= len then 
      if beg = cur then []
      else [String.sub str beg (len - beg)]
    else if char_sep (String.get str cur) 
         then 
      	   let nextw = skip_sep cur in
      	    (String.sub str beg (cur - beg))
      	      ::(split nextw nextw)
	 else split beg (succ cur) in
  let wstart = skip_sep 0 in
  split wstart wstart


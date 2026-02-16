open Charset
open Wchar

type t = {
    mutable buffer : wchar array;
    mutable start : int;
    mutable pos : int;
    mutable len : int
  } 

let init_value = UNKNOWN "", ""

let create n = {
   buffer = Array.make n init_value;
   start = 0;
   pos = 0;
   len = n
   }

let reset buf =
    buf.start <- 0;
    buf.pos <- 0

let newsize old added =
  if added < old then old + old
  else old + old + added

let gc buf =
  let len = buf.pos - buf.start in
  Array.blit buf.buffer buf.start buf.buffer 0 len;
  buf.pos <- len;
  buf.start <- 0

let output_array buf s =
  try
  let l = Array.length s in
  if buf.pos - buf.start + l > buf.len then begin (* no mean of gc *)
    let size = newsize buf.len l in
    let news = Array.make size init_value in
      Array.blit buf.buffer buf.start news 0 (buf.pos - buf.start);
      buf.buffer <- news;
      buf.len <- size;
      buf.pos <- buf.pos - buf.start;
      buf.start <- 0
  end else if buf.pos + l > buf.len then begin (* gc solve the problem *)
    gc buf
  end;
  Array.blit s 0 buf.buffer buf.pos l;
  buf.pos <- buf.pos + l
  with _ -> raise (Failure "output_array")

let output_one buf c =
  try
  if buf.pos - buf.start >= buf.len then begin (* no mean of gc *)
    let size = newsize buf.len 1 in
    let news = Array.make size init_value in
      Array.blit buf.buffer buf.start news 0 (buf.pos - buf.start);
      buf.buffer <- news;
      buf.len <- size;
      buf.pos <- buf.pos - buf.start;
      buf.start <- 0
  end else if buf.pos >= buf.len then begin (* gc solve the problem *)
    gc buf
  end;
  buf.buffer.(buf.pos) <- c;
  buf.pos <- buf.pos + 1
  with _ -> raise (Failure "output_one")

let output buf s ofs l =
  try
  if buf.pos - buf.start + l > buf.len then begin (* no mean of gc *)
    let size = newsize buf.len l in
    let news = Array.make size init_value in
      Array.blit buf.buffer buf.start news 0 (buf.pos - buf.start);
      buf.buffer <- news;
      buf.len <- size;
      buf.pos <- buf.pos - buf.start;
      buf.start <- 0
  end else if buf.pos + l > buf.len then begin (* gc solve the problem *)
    gc buf
  end;
  Array.blit s ofs buf.buffer buf.pos l;
  buf.pos <- buf.pos + l
  with _ -> raise (Failure "output")

let get buf = 
  let dest = Array.make (buf.pos - buf.start) init_value in
  Array.blit buf.buffer buf.start dest 0 (buf.pos - buf.start);
  dest

let used buf =
  buf.pos - buf.start

let input buf str off len =
  if len >= buf.pos - buf.start then begin
    Array.blit buf.buffer buf.start str off (buf.pos - buf.start);
    let l = buf.pos - buf.start in
    reset buf;
    l
  end else begin
    Array.blit buf.buffer buf.start str off len;
    buf.start <- buf.start + len;
    len
  end

let hd buf = 
  try
    buf.buffer.(buf.start)
  with _ -> raise (Failure (Printf.sprintf "hd %d %d" buf.start buf.pos))

let skip1 buf = buf.start <- buf.start + 1 (* unsafe *)


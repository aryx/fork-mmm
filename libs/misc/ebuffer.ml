(*s: commons/ebuffer.ml *)
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

(*s: type [[Ebuffer.t]] *)
(* Extensible buffers *)
type t = {
    mutable buffer : bytes;
    mutable pos : int;
    mutable len : int}
(*e: type [[Ebuffer.t]] *)

(*s: function [[Ebuffer.create]] *)
let create n = {
   buffer = Bytes.create n;
   pos = 0;
   len = n
   }
(*e: function [[Ebuffer.create]] *)

(*s: function [[Ebuffer.reset]] *)
let reset buf =
    buf.pos <- 0
(*e: function [[Ebuffer.reset]] *)

(*s: function [[Ebuffer.newsize]] *)
let newsize old added =
  if added < old then old + old
  else old + old + added
(*e: function [[Ebuffer.newsize]] *)

(*s: function [[Ebuffer.output_string]] *)
let output_string buf s =
  let l = String.length s in
  if buf.pos + l > buf.len then begin
    let size = newsize buf.len l in
    let news = Bytes.create size in
      Bytes.unsafe_blit buf.buffer 0 news 0 buf.pos;
      buf.buffer <- news;
      buf.len <- size
    end;
  Bytes.unsafe_blit_string s 0 buf.buffer buf.pos l;
  buf.pos <- buf.pos + l
(*e: function [[Ebuffer.output_string]] *)
(*s: function [[Ebuffer.output_char]] *)
let output_char buf c =
  if buf.pos >= buf.len then begin
    let size = newsize buf.len 1 in
    let news = Bytes.create size in
      Bytes.unsafe_blit buf.buffer 0 news 0 buf.pos;
      buf.buffer <- news;
      buf.len <- size
    end;
  Bytes.set buf.buffer buf.pos c;
  buf.pos <- buf.pos + 1
(*e: function [[Ebuffer.output_char]] *)

(*s: function [[Ebuffer.output]] *)
let output buf s ofs l =
  if buf.pos + l > buf.len then begin
    let size = newsize buf.len l in
    let news = Bytes.create size in
      Bytes.unsafe_blit buf.buffer 0 news 0 buf.pos;
      buf.buffer <- news;
      buf.len <- size
    end;
  String.blit s ofs buf.buffer buf.pos l;
  buf.pos <- buf.pos + l
(*e: function [[Ebuffer.output]] *)

(*s: function [[Ebuffer.get]] *)
let get buf = 
  Bytes.sub_string buf.buffer 0 buf.pos
(*e: function [[Ebuffer.get]] *)

(*s: function [[Ebuffer.used]] *)
let used buf =
  buf.pos
(*e: function [[Ebuffer.used]] *)
(*e: commons/ebuffer.ml *)

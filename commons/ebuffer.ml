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

(* $Id: ebuffer.ml,v 1.1 1996/10/22 13:12:41 rouaix Exp $ *)

(* Extensible buffers *)
type t = {
    mutable buffer : string;
    mutable pos : int;
    mutable len : int}

let create n = {
   buffer = String.create n;
   pos = 0;
   len = n
   }

let reset buf =
    buf.pos <- 0

let newsize old added =
  if added < old then old + old
  else old + old + added

let output_string buf s =
  let l = String.length s in
  if buf.pos + l > buf.len then begin
    let size = newsize buf.len l in
    let news = String.create size in
      String.unsafe_blit buf.buffer 0 news 0 buf.pos;
      buf.buffer <- news;
      buf.len <- size
    end;
  String.unsafe_blit s 0 buf.buffer buf.pos l;
  buf.pos <- buf.pos + l
let output_char buf c =
  if buf.pos >= buf.len then begin
    let size = newsize buf.len 1 in
    let news = String.create size in
      String.unsafe_blit buf.buffer 0 news 0 buf.pos;
      buf.buffer <- news;
      buf.len <- size
    end;
  buf.buffer.[buf.pos] <- c;
  buf.pos <- buf.pos + 1

let output buf s ofs l =
  if buf.pos + l > buf.len then begin
    let size = newsize buf.len l in
    let news = String.create size in
      String.unsafe_blit buf.buffer 0 news 0 buf.pos;
      buf.buffer <- news;
      buf.len <- size
    end;
  String.blit s ofs buf.buffer buf.pos l;
  buf.pos <- buf.pos + l


let get buf = 
  String.sub buf.buffer 0 buf.pos

let used buf =
  buf.pos
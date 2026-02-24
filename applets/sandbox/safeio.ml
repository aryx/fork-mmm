(***********************************************************************)
(*                                                                     *)
(*                           Caml Applets                              *)
(*                                                                     *)
(*          Francois Rouaix, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Capabilities

module GetIO(C: sig val capabilities: t end) = 
  struct

  let ask = ask C.capabilities
  
type in_channel = {
  in_channel : Pervasives.in_channel;
  mutable in_status : bool
  }
type out_channel = {
  out_channel : Pervasives.out_channel;
  mutable out_status : bool
  }

type open_flag = Pervasives.open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock

let denied () = raise (Sys_error (I18n.sprintf "Permission denied"))

(* Do *not* give standard channels stdin/stdout/stderr *)

let open_out name =
  let name = String.copy name in
  if ask (FileW name)
  then {out_channel = open_out name; out_status = true}
  else denied()

let open_out_bin name =
  let name = String.copy name in
  if ask (FileW name)
  then {out_channel = open_out_bin name;
      	out_status = true}
  else denied()

let open_out_gen flags mode name =
  let name = String.copy name in
  if ask (FileW name)
  then {out_channel = open_out_gen flags mode name;
      	out_status = true}
  else denied()

let flush woc = 
  if woc.out_status then flush woc.out_channel
  else invalid_arg "flush"

let output_char woc c = 
  if woc.out_status then output_char woc.out_channel c
  else invalid_arg "output_char"

let output_string woc s =
  if woc.out_status then output_string woc.out_channel s
  else invalid_arg "output_string"

let output woc buff ofs len =
  if woc.out_status then output woc.out_channel buff ofs len
  else invalid_arg "output"

let output_byte woc n = 
  if woc.out_status then output_byte woc.out_channel n
  else invalid_arg "output_byte"

let output_binary_int woc n =
  if woc.out_status then output_binary_int woc.out_channel n
  else invalid_arg "output_binary_int"

let output_value woc v =
  if woc.out_status then output_value woc.out_channel v
  else invalid_arg "output_value"

let seek_out woc n =
  if woc.out_status then seek_out woc.out_channel n
  else invalid_arg "seek_out"

let pos_out woc =
  if woc.out_status then pos_out woc.out_channel
  else invalid_arg "pos_out"

let out_channel_length woc =
  if woc.out_status then out_channel_length woc.out_channel
  else invalid_arg "out_channel_length"

let close_out woc = 
  if woc.out_status then begin
    woc.out_status <- false;
    close_out woc.out_channel
    end
  else invalid_arg "n.close_out"


(* General input functions *)

let open_in name =
  let name = String.copy name in
  if ask (FileR name)
  then {in_channel = open_in name; in_status = true}
  else denied()

let open_in_bin name =
  let name = String.copy name in
  if ask (FileR name)
  then {in_channel = open_in_bin name; in_status = true}
  else denied()

let open_in_gen flags mode name =
  let name = String.copy name in
  if ask (FileR name)
  then {in_channel = open_in_gen flags mode name; in_status = true}
  else denied()

let input_char wic =
  if wic.in_status then input_char wic.in_channel
  else invalid_arg "input_char"

let input_line wic =
  if wic.in_status then input_line wic.in_channel
  else invalid_arg "input_line"

let input wic buf ofs len =
  if wic.in_status then input wic.in_channel buf ofs len
  else invalid_arg "input"

let really_input wic buf ofs len =
  if wic.in_status then really_input wic.in_channel buf ofs len
  else invalid_arg "really_input"

let input_byte wic =
  if wic.in_status then input_byte wic.in_channel
  else invalid_arg "input_byte"

let input_binary_int wic =
  if wic.in_status then input_binary_int wic.in_channel
  else invalid_arg "input_binary_int"

(* input_value is unsafe *)
let seek_in wic n =
  if wic.in_status then seek_in wic.in_channel n
  else invalid_arg "seek_in"

let pos_in wic =
  if wic.in_status then pos_in wic.in_channel
  else invalid_arg "pos_in"

let in_channel_length wic =
  if wic.in_status then in_channel_length wic.in_channel
  else invalid_arg "in_channel_length"

let close_in wic = 
  if wic.in_status then begin
    wic.in_status <- false;
    close_in wic.in_channel
    end
  else invalid_arg "close_in"

end

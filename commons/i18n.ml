(*s: ./commons/i18n.ml *)
open Tk

(*s: function I18n.fprintf *)
(* Internationalization (translation of error messages) *)

let fprintf x = 
  if !Lang.japan then I18nprintf.fprintf x else Printf.fprintf x
(*e: function I18n.fprintf *)
(*s: function I18n.sprintf *)
let sprintf x = 
  if !Lang.japan then I18nprintf.sprintf x else Printf.sprintf x
(*e: function I18n.sprintf *)

(*s: constant I18n.language *)
let language = ref ""
(*e: constant I18n.language *)
(*s: constant I18n.message_file *)
let message_file = ref ""
(*e: constant I18n.message_file *)

(*s: function I18n.read_transl_file *)
let read_transl_file msgfile =
  let ic = open_in msgfile in
  let tag_buffer = String.create 16
  and msg_buffer = String.create 1024 in
  let rec store_tag c i =
    if i >= 16 then i else (tag_buffer.[i] <- c; succ i)
  and store_msg c i =
    if i >= 1024 then i else (msg_buffer.[i] <- c; succ i)
  and read_line i =
    match input_char ic with
      '\n' -> i
    | '\\' -> begin match input_char ic with
                '\\' -> read_line(store_msg '\\' i)
              | 'n'  -> read_line(store_msg '\n' i)
              | '\n' -> skip_blanks i
              | c    -> read_line(store_msg c (store_msg '\\' i))
              end
    | c    -> read_line(store_msg c i)
  and skip_blanks i =
    match input_char ic with
      ' '|'\t' -> skip_blanks i
    | c        -> read_line(store_msg c i)
  and read_tag i =
    match input_char ic with
      ':'           -> (i, skip_blanks 0)
    | ' '|'\n'|'\t' -> read_tag i
    | c             -> read_tag(store_tag c i) in
  let transl_tbl = Hashtbl.create 37 in
  let currsrc = ref "" in
  begin try
    while true do
      let (tag_len, msg_len) = read_tag 0 in
      if String.sub tag_buffer 0 tag_len = "src" then
        currsrc := String.sub msg_buffer 0 msg_len
      else if String.sub tag_buffer 0 tag_len = !language then
        Hashtbl.add transl_tbl !currsrc (String.sub msg_buffer 0 msg_len)
      else ()
    done
  with End_of_file ->
    close_in ic
  end;
  transl_tbl
(*e: function I18n.read_transl_file *)

(*s: enum I18n.translation_table *)
type translation_table =
    Unknown
  | NoTranslation
  | Transl of (string, string) Hashtbl.t
(*e: enum I18n.translation_table *)

(*s: constant I18n.transl_table *)
let transl_table = ref Unknown
(*e: constant I18n.transl_table *)

(*s: function I18n.translate *)
let rec translate msg =
  match !transl_table with
    NoTranslation ->
      msg
  | Transl tbl ->
      begin try Hashtbl.find tbl msg with Not_found -> msg end
  | Unknown ->
      transl_table :=
        if String.length !language == 0 then
          NoTranslation
        else begin
          try
            if Sys.file_exists !message_file then	   
              Transl(read_transl_file !message_file)
            else NoTranslation
          with Sys_error _ | Sys.Break ->
            NoTranslation
        end;
      translate msg
(*e: function I18n.translate *)

(*s: function I18n.fprintf (./commons/i18n.ml) *)
let fprintf oc (fmt : ('a, out_channel, unit) format) =
  fprintf oc
(*e: function I18n.fprintf (./commons/i18n.ml) *)
    (Obj.magic(translate(Obj.magic fmt : string)) :
                                ('a, out_channel, unit) format)

(*s: function I18n.sprintf (./commons/i18n.ml) *)
let sprintf (fmt : ('a, unit, string) format) =
  sprintf
(*e: function I18n.sprintf (./commons/i18n.ml) *)
    (Obj.magic(translate(Obj.magic fmt : string)) :
                                ('a, unit, string) format)

let printf fmt = fprintf stdout fmt
and eprintf fmt = fprintf stderr fmt

(*s: function I18n.menu_option *)
let menu_option l =
  let under_pos = ref (-1) 
  and text = ref ""
  in
    List.iter (function 
    UnderlinedChar x -> under_pos := x
      | Text x  -> text := x
      | Label x -> text := x
      |	_ -> () ) l;
    let trans = translate !text in
      let new_text, new_under_pos =
    if !text = trans then !text, !under_pos
    else
      if !under_pos = 0 & 
        String.get !text 0 = String.get trans 0 then trans, 0
      else
        (String.make 1 (String.get !text !under_pos)) ^ ":" ^ trans, 0 
      in
        List.map (function
        UnderlinedChar _ -> UnderlinedChar new_under_pos
      | Text x -> Text new_text
      | Label x -> Label new_text
      | x -> x ) l
(*e: function I18n.menu_option *)

(*s: exception I18n.Found *)
exception Found of string
(*e: exception I18n.Found *)

(*s: function I18n.menu_pattern *)
let menu_pattern l =
  let l' = menu_option l in
  try
    List.iter (function
      | Text x -> raise (Found x)
      | Label x -> raise (Found x)
      | _ -> ()) l';
    raise (Failure "I18n.menu_pattern : the option list contains no text")
  with
    Found x -> x
(*e: function I18n.menu_pattern *)
(*e: ./commons/i18n.ml *)

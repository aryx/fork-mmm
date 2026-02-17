(*s: commons/i18n.ml *)

(*s: function [[I18n.fprintf]] *)
(* Internationalization (translation of error messages) *)

let fprintf x = 
  Printf.fprintf x
(*e: function [[I18n.fprintf]] *)
(*s: function [[I18n.sprintf]] *)
let sprintf x = 
  Printf.sprintf x
(*e: function [[I18n.sprintf]] *)

(*s: constant [[I18n.language]] *)
let language = ref ""
(*e: constant [[I18n.language]] *)
(*s: constant [[I18n.message_file]] *)
let message_file = ref ""
(*e: constant [[I18n.message_file]] *)

(*s: function [[I18n.read_transl_file]] *)
let read_transl_file msgfile =
  let ic = open_in msgfile in
  let tag_buffer = Bytes.create 16
  and msg_buffer = Bytes.create 1024 in
  let rec store_tag c i =
    if i >= 16 then i else (Bytes.set tag_buffer i c; succ i)
  and store_msg c i =
    if i >= 1024 then i else (Bytes.set msg_buffer i c; succ i)
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
      if Bytes.sub_string tag_buffer 0 tag_len = "src" then
        currsrc := Bytes.sub_string msg_buffer 0 msg_len
      else if Bytes.sub_string tag_buffer 0 tag_len = !language then
        Hashtbl.add transl_tbl !currsrc (Bytes.sub_string msg_buffer 0 msg_len)
      else ()
    done
  with End_of_file ->
    close_in ic
  end;
  transl_tbl
(*e: function [[I18n.read_transl_file]] *)

(*s: type [[I18n.translation_table]] *)
type translation_table =
    Unknown
  | NoTranslation
  | Transl of (string, string) Hashtbl.t
(*e: type [[I18n.translation_table]] *)

(*s: constant [[I18n.transl_table]] *)
let transl_table = ref Unknown
(*e: constant [[I18n.transl_table]] *)

(*s: function [[I18n.translate]] *)
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
(*e: function [[I18n.translate]] *)

(*s: function I18n.fprintf (./commons/i18n.ml) *)
let fprintf oc (fmt : ('a, out_channel, unit) format) =
  fprintf oc
    (Obj.magic(translate(Obj.magic fmt : string)) :
                                ('a, out_channel, unit) format)
(*e: function I18n.fprintf (./commons/i18n.ml) *)

(*s: function I18n.sprintf (./commons/i18n.ml) *)
let sprintf (fmt : ('a, unit, string) format) =
  sprintf
    (Obj.magic(translate(Obj.magic fmt : string)) :
                                ('a, unit, string) format)

(*e: function I18n.sprintf (./commons/i18n.ml) *)

let s_ fmt = sprintf fmt

let printf fmt = fprintf stdout fmt
and eprintf fmt = fprintf stderr fmt

(*s: function [[I18n.menu_option]] *)
(*e: function [[I18n.menu_option]] *)

(*s: exception [[I18n.Found]] *)
(*e: exception [[I18n.Found]] *)

(*s: function [[I18n.menu_pattern]] *)
(*e: function [[I18n.menu_pattern]] *)
(*e: commons/i18n.ml *)

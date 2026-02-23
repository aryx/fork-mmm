val debug : bool ref

type code = | JIS | EUC | SJIS | ISO8859 | UTF8 | Binary | Unknown

val encode_table : (string * code) list

type detected_code = | Code of code | Mixed

val detected_code_names : (detected_code * string) list

type decoder =
  | DecoderASCII
  | DecoderJIS
  | DecoderEUCorSJIS
  | DecoderEUC
  | DecoderSJIS
  | DecoderISO8859
  | DecoderUTF8

type read_type = bytes -> int -> int -> int

class read_native : (read_type) -> object
  method read : read_type
end

val esc_sequence : Charset.charset -> string

class read_i18n : (read_type) -> object
  method set_code : code -> unit
  method get_code : detected_code
  method read : read_type
end

val input_from_string : string -> read_type

type japanese_config = {
    mutable decoders : decoder list option;
  }
 
val default_config : unit -> japanese_config

val change_to_jis : japanese_config -> unit
val change_to_sjis : japanese_config -> unit
val change_to_euc : japanese_config -> unit
val change_to_iso8859 : japanese_config -> unit
val change_to_utf8 : japanese_config -> unit
val change_to_autodetect : japanese_config -> unit

val create_read_native : read_type -> read_i18n
val create_read_junet : read_type -> read_i18n
val create_read_japanese : read_type -> japanese_config -> read_i18n

val encoder : detected_code -> string -> string

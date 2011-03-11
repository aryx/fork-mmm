(* String utilities *)
val split_str : (char -> bool) -> string -> string list
val get_suffix : string -> string

val hex_to_dec : char -> int
val dec_to_hex : int -> char

val hex_to_string : string -> string

val gensym : string -> string
val egensym : string -> unit -> string

val rem_trailing_sp : string -> string

val catenate_sep : string -> string list -> string

val norm_crlf : bool -> string -> int -> int -> string * bool
    (* [norm_crlf last_was_cr buf offs len] returns
       buf with CRLF/CR/LF converted to LF, and a flag indicating
       whether last char was CR *)

(*s: commons/mstring.mli *)
(*s: signature [[Mstring.split_str]] *)
(* String utilities *)
val split_str : (char -> bool) -> string -> string list
(*e: signature [[Mstring.split_str]] *)
(*s: signature [[Mstring.get_suffix]] *)
val get_suffix : string -> string
(*e: signature [[Mstring.get_suffix]] *)

(*s: signature [[Mstring.hex_to_dec]] *)
val hex_to_dec : char -> int
(*e: signature [[Mstring.hex_to_dec]] *)
(*s: signature [[Mstring.dec_to_hex]] *)
val dec_to_hex : int -> char
(*e: signature [[Mstring.dec_to_hex]] *)

(*s: signature [[Mstring.hex_to_string]] *)
val hex_to_string : string -> string
(*e: signature [[Mstring.hex_to_string]] *)

(*s: signature [[Mstring.gensym]] *)
val gensym : string -> string
(*e: signature [[Mstring.gensym]] *)
(*s: signature [[Mstring.egensym]] *)
val egensym : string -> unit -> string
(*e: signature [[Mstring.egensym]] *)

(*s: signature [[Mstring.rem_trailing_sp]] *)
val rem_trailing_sp : string -> string
(*e: signature [[Mstring.rem_trailing_sp]] *)

(*s: signature [[Mstring.catenate_sep]] *)
val catenate_sep : string -> string list -> string
(*e: signature [[Mstring.catenate_sep]] *)

(*s: signature [[Mstring.norm_crlf]] *)
val norm_crlf : bool -> string -> int -> int -> string * bool
    (* [norm_crlf last_was_cr buf offs len] returns
       buf with CRLF/CR/LF converted to LF, and a flag indicating
       whether last char was CR *)
(*e: signature [[Mstring.norm_crlf]] *)
(*e: commons/mstring.mli *)

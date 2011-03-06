module Jtk : sig
  open Tk
  open Widget

  type jtkOptions =
	KanjiFont of string

  type kanjiCode =
	  ANY		(* tk option: ANY *)
	| EUC		(* tk option: EUC *)
	| JIS		(* tk option: JIS *)
	| SJIS		(* tk option: SJIS *)

  module Kanji : sig
    val canvas_item : widget -> tagOrId -> jtkOptions list -> unit 
    val code : string -> kanjiCode 
    val conversion : kanjiCode -> kanjiCode -> string -> string 
    val internal_code_get : kanjiCode -> kanjiCode 
    val menu_entry : widget -> index -> jtkOptions list -> unit 
    val string_length : string -> int 
    val text_tag : widget -> textTag -> jtkOptions list -> unit 
    val widget_kanjifont : widget -> jtkOptions list -> unit 
  end
end

(*s: ./commons/i18n.mli *)
(*s: signature I18n.message_file *)
val message_file : string ref
(*e: signature I18n.message_file *)
(*s: signature I18n.language *)
val language : string ref
(*e: signature I18n.language *)

(*s: signature I18n.sprintf *)
val sprintf: ('a, unit, string) format -> 'a
(*e: signature I18n.sprintf *)
(* pad's compact alias *)
val s_: ('a, unit, string) format -> 'a

(*s: signature I18n.menu_option *)
(*e: signature I18n.menu_option *)
(*s: signature I18n.menu_pattern *)
(*e: signature I18n.menu_pattern *)

val translate: string -> string
(*e: ./commons/i18n.mli *)

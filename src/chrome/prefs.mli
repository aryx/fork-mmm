(*s: gui/prefs.mli *)
(*s: type [[Prefs.pref_type]] *)
(* Exported so that we can plug applet preferences *)
type pref_type =
 | Bool of bool ref
 | String of string ref
 | Int of int ref
 | Float of float ref
 | AbstractType of (Textvariable.textVariable -> unit) * 
                   (Textvariable.textVariable -> unit)
                   (* init, set *)
(*e: type [[Prefs.pref_type]] *)

(*s: type [[Prefs.pref]] *)
type pref = {
  packed_widget : Widget.widget;
  pref_variable : Textvariable.textVariable;
  pref_type : pref_type;
  pref_name : string; (* shall not contain : *)
  resource_name : string (* shall not contain : *)
}
(*e: type [[Prefs.pref]] *)

module PrefMap : Map.S with type key = string

(*s: type [[Prefs.pref_family]] *)
(* A family of preferences *)
type pref_family =
  {family_widget: Widget.widget;
   family_init : unit -> unit;
   family_save : unit -> string PrefMap.t;
   family_load : unit -> unit;
   family_title : string
  }
(*e: type [[Prefs.pref_family]] *)

(*s: signature [[Prefs.bool_pref]] *)
val bool_pref : string -> bool ref -> Widget.widget -> pref
(*e: signature [[Prefs.bool_pref]] *)
(*s: signature [[Prefs.int_pref]] *)
val int_pref : string -> int ref -> Widget.widget -> pref
(*e: signature [[Prefs.int_pref]] *)
(*s: signature [[Prefs.float_pref]] *)
val float_pref : string -> float ref -> Widget.widget -> pref
(*e: signature [[Prefs.float_pref]] *)
(*s: signature [[Prefs.string_pref]] *)
val string_pref : string -> string ref -> Widget.widget -> pref
    (* [<type>_pref name internal_location top] *)
(*e: signature [[Prefs.string_pref]] *)

(*s: signature [[Prefs.option_pref]] *)
val option_pref :
    string ->
    (Textvariable.textVariable -> unit) *
    (Textvariable.textVariable -> unit) * string list ->
    Widget.widget -> pref
(*e: signature [[Prefs.option_pref]] *)


(*s: signature [[Prefs.abstract_bool_pref]] *)
val abstract_bool_pref :
    string ->
      (Textvariable.textVariable -> unit) ->
      (Textvariable.textVariable -> unit) -> Widget.widget -> pref
(*e: signature [[Prefs.abstract_bool_pref]] *)

(*s: signature [[Prefs.abstract_string_pref]] *)
val abstract_string_pref :
    string ->
      (Textvariable.textVariable -> unit) ->
      (Textvariable.textVariable -> unit) -> Widget.widget -> pref
(*e: signature [[Prefs.abstract_string_pref]] *)


(*s: signature [[Prefs.option_handlers]] *)
val option_handlers :
    ('a * string) list ->
    (unit -> 'a) ->
    ('a -> unit) ->
    (Textvariable.textVariable -> unit) * (Textvariable.textVariable -> unit) *
    string list
(*e: signature [[Prefs.option_handlers]] *)



(*s: signature [[Prefs.family]] *)
val family :
    Widget.widget -> string -> (Widget.widget -> pref) list -> pref_family
(*e: signature [[Prefs.family]] *)

(*s: signature [[Prefs.pref_error]] *)
val pref_error : string -> unit
(*e: signature [[Prefs.pref_error]] *)

(*s: signature [[Prefs.resource_name]] *)
val resource_name : string -> string
(*e: signature [[Prefs.resource_name]] *)

(*s: signature [[Prefs.define]] *)
val define :
    Fpath.t ->
    (Widget.widget -> pref_family) list -> (unit -> unit) list -> unit -> unit
    (* [define filename pref_builders pref_mute]
       returns a function that displays the preference panel
     *)
(*e: signature [[Prefs.define]] *)
(*e: gui/prefs.mli *)

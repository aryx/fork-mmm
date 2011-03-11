(* Exported so that we can plug applet preferences *)
type pref_type =
   Bool of bool ref
 | String of string ref
 | Int of int ref
 | Float of float ref
 | AbstractType of (Textvariable.textVariable -> unit) * 
      	       	 (Textvariable.textVariable -> unit)
                 (* init, set *)

type pref = {
  packed_widget : Widget.widget;
  pref_variable : Textvariable.textVariable;
  pref_type : pref_type;
  pref_name : string; (* shall not contain : *)
  resource_name : string (* shall not contain : *)
}

module PrefMap : Map.S with type key = string

(* A family of preferences *)
type pref_family =
  {family_widget: Widget.widget;
   family_init : unit -> unit;
   family_save : unit -> string PrefMap.t;
   family_load : unit -> unit;
   family_title : string
  }

val bool_pref : string -> bool ref -> Widget.widget -> pref
val int_pref : string -> int ref -> Widget.widget -> pref
val float_pref : string -> float ref -> Widget.widget -> pref
val string_pref : string -> string ref -> Widget.widget -> pref
    (* [<type>_pref name internal_location top] *)

val option_pref :
    string ->
    (Textvariable.textVariable -> unit) *
    (Textvariable.textVariable -> unit) * string list ->
    Widget.widget -> pref


val abstract_bool_pref :
    string ->
      (Textvariable.textVariable -> unit) ->
      (Textvariable.textVariable -> unit) -> Widget.widget -> pref

val abstract_string_pref :
    string ->
      (Textvariable.textVariable -> unit) ->
      (Textvariable.textVariable -> unit) -> Widget.widget -> pref


val option_handlers :
    ('a * string) list ->
    (unit -> 'a) ->
    ('a -> unit) ->
    (Textvariable.textVariable -> unit) * (Textvariable.textVariable -> unit) *
    string list



val family :
    Widget.widget -> string -> (Widget.widget -> pref) list -> pref_family

val pref_error : string -> unit

val resource_name : string -> string

val define :
    string ->
    (Widget.widget -> pref_family) list -> (unit -> unit) list -> unit -> unit
    (* [define filename pref_builders pref_mute]
       returns a function that displays the preference panel
     *)

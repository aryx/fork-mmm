(* Support for widget manipulations *)

type widget
  (* widget is an abstract type *)

val default_toplevel : widget
  (* [default_toplevel] is "." in Tk, the toplevel widget that is
     always existing during a Tk session. Destroying [default_toplevel]
     ends the main loop
   *)

val atom : widget -> string -> widget
  (* [atom parent name] returns the widget [parent.name]. The widget is
     not created. Only its name is returned. In a given parent, there may
     only exist one children for a given name.
     This function should only be used to check the existence of a widget
     with a known name. It doesn't add the widget to the internal tables
     of CamlTk.
   *)

val name : widget -> string
  (* [name w] returns the name (tk "path") of a widget *)

(*--*)
(* The following functions are used internally.
   There is normally no need for them in users programs
 *)

val known_class : widget -> string
  (* [known_class w] returns the class of a widget (e.g. toplevel, frame),
     as known by the CamlTk interface.
     Not equivalent to "winfo w" in Tk.
   *)

val dummy : widget
  (* [dummy] is a widget used as context when we don't have any.
     It is *not* a real widget.
   *)
          
val new_atom : string -> widget -> widget
val new_named : string -> widget -> string -> widget
  (* Abstract creation functions
     [new_atom class parent]
     [new_named class parent name]
   *)

val get_atom : string -> widget
  (* [get_atom path] returns the widget with Tk path [path] *)

val remove : widget -> unit
  (* [remove w] removes widget from the internal tables *)

(* Subtypes tables *)
val widget_any_table : string list
val widget_button_table : string list
val widget_canvas_table : string list
val widget_checkbutton_table : string list
val widget_entry_table : string list
val widget_frame_table : string list
val widget_label_table : string list
val widget_listbox_table : string list
val widget_menu_table : string list
val widget_menubutton_table : string list
val widget_message_table : string list
val widget_radiobutton_table : string list
val widget_scale_table : string list
val widget_scrollbar_table : string list
val widget_text_table : string list
val widget_toplevel_table : string list

val chk_sub : string -> 'a list -> 'a -> unit
val check_class : widget -> string list -> unit
      (* Widget subtyping *)

exception IllegalWidgetType of string
      (* Raised when widget command applied illegally*)

(*
 * Widgets
 *)

exception IllegalWidgetType of string
      (* Raised when widget command applied illegally*)

type widget =
  Untyped of string
| Typed of string * string

let table = (Hashtbl.create 401 : (string,widget) Hashtbl.t)

let name = function
    Untyped s -> s
 |  Typed (s,_) -> s

(* Normally all widgets are known *)
(* this is a provision for send commands to external tk processes *)
let known_class = function
    Untyped _ -> "unknown"
  | Typed (_,c) -> c

(* This one is always created by opentk *)
let default_toplevel =
  let wname = "." in
  let w = Typed (wname, "toplevel") in
    Hashtbl.add table wname w;
    w

(* Dummy widget to which global callbacks are associated *)
(* also passed around by camltotkoption when no widget in context *)
let dummy = 
  Untyped "dummy"

let remove w =
  Hashtbl.remove table (name w)

(* Retype widgets returned from Tk *)
(* JPF report: sometime s is "", see Protocol.cTKtoCAMLwidget *)
let get_atom s =
  try
    Hashtbl.find table s
  with
    Not_found -> Untyped s

let naming_scheme = [
        "button", "b";
	"canvas", "ca";
	"checkbutton", "cb";
	"entry", "en";
        "frame", "f";
	"label", "l";
	"listbox", "li";
	"menu", "me";
	"menubutton", "mb";
	"message", "ms";
        "radiobutton", "rb";
	"scale", "sc";
	"scrollbar", "sb";
	"text", "t";
      	"toplevel", "top" ]


let widget_any_table =  List.map fst naming_scheme
(* subtypes *)
let widget_button_table = [ "button" ]
and widget_canvas_table = [ "canvas" ]
and widget_checkbutton_table = [ "checkbutton" ]
and widget_entry_table = [ "entry" ]
and widget_frame_table = [ "frame" ]
and widget_label_table = [ "label" ]
and widget_listbox_table = [ "listbox" ]
and widget_menu_table = [ "menu" ]
and widget_menubutton_table = [ "menubutton" ]
and widget_message_table = [ "message" ]
and widget_radiobutton_table = [ "radiobutton" ]
and widget_scale_table = [ "scale" ]
and widget_scrollbar_table = [ "scrollbar" ]
and widget_text_table = [ "text" ]
and widget_toplevel_table = [ "toplevel" ]

let new_suffix clas n =
  try 
    (List.assoc clas naming_scheme) ^ (string_of_int n)
  with
    Not_found -> "w" ^ (string_of_int n)
  

(* 
 * There is a leak in Tk for atom names in widgets (as well as for
 * color names and some other atoms). The leak is in Tk_GetUid.
 * We should thus avoid generating arbitrary names for widgets.
 *)

(* The function called by generic creation *)
let new_atom =
  let counter = ref 0 in
  fun clas parent ->
      let parentpath = name parent in
      let path = 
      	 incr counter;
	 if parentpath = "."
	 then "." ^ (new_suffix clas !counter)
	 else parentpath ^ "." ^ (new_suffix clas !counter)
        in
      let w = Typed(path,clas) in
	Hashtbl.add table path w;
	w


let new_named clas parent pathcomp =
  let parentpath = name parent in
  let path =
    if parentpath = "."
    then "." ^ pathcomp
    else parentpath ^ "." ^ pathcomp in
  let w = Typed(path,clas) in
	Hashtbl.add table path w;
	w
  
(* Just create a path. Only to check existence of widgets *)
(* Use with care *)
let atom parent pathcomp =
  let parentpath = name parent in
  let path =
    if parentpath = "."
    then "." ^ pathcomp
    else parentpath ^ "." ^ pathcomp in
      Untyped path



(* Redundant with subtyping of Widget, backward compatibility *)
let check_class w clas =
  match w with
    Untyped _ -> () (* assume run-time check by tk*)
  | Typed(_,c) ->
       	 if List.mem c clas then ()
      	 else raise (IllegalWidgetType c)


(* Checking membership of constructor in subtype table *)
let chk_sub errname table c =
  if List.mem c table then ()
  else raise (Invalid_argument errname)

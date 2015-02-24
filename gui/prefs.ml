(*s: ./gui/prefs.ml *)
(* Preferences *)
open Tk
open Mstring
open Fontprefs

(*s: function Prefs.pref_error *)
(* Generic report *)
let pref_error msg =
   ignore (
     Frx_dialog.f Widget.default_toplevel (gensym "error")
      (I18n.sprintf "Preference Error") 
      msg
      (Predefined "") 0 [I18n.sprintf "Ok"])
(*e: function Prefs.pref_error *)

(*s: function Prefs.resource_name *)
(* Converts an arbitrary string to a name suitable as a "global" resource *)
let resource_name pref_name =
  let words = Mstring.split_str (function ' ' -> true | _ -> false) pref_name 
  in
  (* for each words, remove non alpha-numerics *)
  (* in addition, make the each first characters capital *)
  let words' = List.map (fun word ->
    let buf = String.create (String.length word) in
    let pos = ref 0 in
    for i = 0 to String.length word - 1 do
      if   ('A' <= word.[i] && word.[i] <= 'Z') 
    or ('a' <= word.[i] && word.[i] <= 'z') 
    or ('0' <= word.[i] && word.[i] <= '9') then begin
        buf.[!pos] <- word.[i];
        incr pos
    end;
    done;
    let x = String.sub buf 0 !pos in
    begin 
      try
    if 'a' <= x.[0] && x.[0] <= 'z' then
      x.[0] <- Char.chr (Char.code x.[0] + Char.code 'A' - Char.code 'a');
      with
    Invalid_argument _ -> 
      (* Strangely, x could be "". *) ()
    end;
    x ) words
  in
    "pref" ^ String.concat "" words'
(*e: function Prefs.resource_name *)

(*s: constant Prefs.class_name *)
let class_name = resource_name 
  (* it is not correct but works *)
(*e: constant Prefs.class_name *)

(*s: enum Prefs.pref_type (./gui/prefs.ml) *)
(*
 * Various predefined preference types
 *)
type pref_type =
   Bool of bool ref
 | String of string ref
 | Int of int ref
 | Float of float ref
 | AbstractType of (Textvariable.textVariable -> unit) * 
      	       	 (Textvariable.textVariable -> unit)
                 (* init, set  as defined below *)
(*e: enum Prefs.pref_type (./gui/prefs.ml) *)

(*s: enum Prefs.pref (./gui/prefs.ml) *)
(*
 * Support for interactive setting of a preference 
 *)
type pref = {
  packed_widget : Widget.widget;	       (* visual feedback *)
  pref_variable : Textvariable.textVariable;   (* placeholder for string
                          version of pref value, and
                          possibly "electric" change *)
  pref_type : pref_type;		       (* internal definition *)
  pref_name : string;	 (* internal name (shall not contain :) *)
  resource_name : string (* resource name (shall not contain :) *)
}
(*e: enum Prefs.pref (./gui/prefs.ml) *)


(*s: function Prefs.init_pref *)
(*
 * Init the Tk variables in the pref editor from the internal 
 * value of the preference (usually a reference)
 *)
let init_pref {pref_type = typ; pref_variable = v} = match typ with
   Bool r -> Textvariable.set v (if !r then "1" else "0")
 | String r -> Textvariable.set v !r
 | Int r ->  Textvariable.set v (string_of_int !r)
 | Float r ->  Textvariable.set v (string_of_float !r)
 | AbstractType(i,_) -> i v
(*e: function Prefs.init_pref *)

(*s: function Prefs.set_pref *)
(* 
 * Set the internal preference value from the editor value (ie textvariable)
 * NOTE: basic predefined types do not allow extra code to run when the
 * value is modified.
 *)
let set_pref {pref_type = typ; pref_variable = v} = match typ with
   Bool r -> r := Textvariable.get v = "1"
 | String r -> r := Textvariable.get v
 | Int r ->
     let s = Textvariable.get v in
      begin try r := int_of_string s
      with Failure "int_of_string" ->
      	     pref_error (I18n.sprintf "Not an integer: %s" s)
      end
 | Float r ->
     let s = Textvariable.get v in
      begin try r := float_of_string s
      with Failure "float_of_string" ->
      	     pref_error (I18n.sprintf "Not a float: %s" s)
      end
 | AbstractType(_,s) -> s v
(*e: function Prefs.set_pref *)

(*s: function Prefs.load_pref *)
(* 
 * Given the current resource database, set the internal and editor values
 * of the preference.
 *)
let load_pref pref =
  try 
    let prefdata = Resource.get Widget.default_toplevel 
      pref.resource_name pref.resource_name (* it is not correct but works *)
    in
    (* ONLY if non-empty ! *)
    if prefdata <> "" then begin
      Textvariable.set pref.pref_variable prefdata;
      set_pref pref
    end
  with
    Not_found -> () (* Never happen if database is complete *)
(*e: function Prefs.load_pref *)

(*s: function Prefs.save_pref *)
(* 
 * Adds the current pref value (from pref editor) to a preference table
 *)
let save_pref add pref =
  add pref.resource_name (Textvariable.get pref.pref_variable)
(*e: function Prefs.save_pref *)


(*s: function Prefs.bool_pref *)
(*
 * Building the preference manager for predefined preference types
 *)


let bool_pref name r top = 
  let v = Textvariable.create_temporary top in
  (* The frame is just to avoid expanding *)
  let f = Frame.create top [] in
  let w = Checkbutton.create f [Text name; Variable v] in
   pack [w][Side Side_Left; Anchor W; Fill Fill_X];
  let p =
    { pref_type = Bool r;
      pref_variable = v;
      packed_widget = f;
      pref_name = name;
      resource_name = resource_name name } in
  (* Automatically perform the preference change when you trigger the button *)
  Checkbutton.configure w [Command (fun () -> set_pref p)];
  p
(*e: function Prefs.bool_pref *)

(*s: function Prefs.int_pref *)
let int_pref name r top = 
  let v = Textvariable.create_temporary top in
  let f,e = Frx_entry.new_labelm_entry top name v in
  let p =
    { pref_type = Int r;
      pref_variable = v;
      packed_widget = f;
      pref_name = name;
      resource_name = resource_name name } in
  (* Automatically perform the preference change when you edit the entry *)
  (* NOTE: we have to use a "tracer" on the variable, since the user does *)
  (* not necessarily type Enter when editing is finished. OTOH, this will *)
  (* cause additionnal invocations during load_pref and init_pref *)
  let rec el () = Textvariable.handle v (fun () -> set_pref p; el()) in
  el(); p
(*e: function Prefs.int_pref *)

(*s: function Prefs.float_pref *)
let float_pref name r top = 
  let v = Textvariable.create_temporary top in
  let f,e = Frx_entry.new_labelm_entry top name v in
  let p = 
    { pref_type = Float r;
      pref_variable = v;
      packed_widget = f;
      pref_name = name;
      resource_name = resource_name name} in
  (* see above *)
  let rec el () = Textvariable.handle v (fun () -> set_pref p; el()) in
  el(); p
(*e: function Prefs.float_pref *)

(*s: function Prefs.string_pref *)
let string_pref name r top = 
  let v = Textvariable.create_temporary top in
  let f,e = Frx_entry.new_labelm_entry top name v in
  let p = 
    { pref_type = String r;
      pref_variable = v;
      packed_widget = f;
      pref_name = name;
      resource_name = resource_name name } in
  (* see above *)
  let rec el () = Textvariable.handle v (fun () -> set_pref p; el()) in
  el(); p
(*e: function Prefs.string_pref *)

(*s: function Prefs.option_pref *)
let option_pref name (i, s, p) top =
  let v = Textvariable.create_temporary top in
  let f = Frame.create top [] in
  let l = Label.create f [Text name]
  and o,_ = Optionmenu.create f v p in
    pack [l;o][Side Side_Left];
  let p = {
    pref_type = AbstractType(i,s);
    pref_variable = v;
    packed_widget = f;
    pref_name = name;
    resource_name = resource_name name} in
  (* see above *)
  let rec el () = Textvariable.handle v (fun () -> set_pref p; el()) in
  el(); p
(*e: function Prefs.option_pref *)


(*s: function Prefs.abstract_bool_pref *)
(*
 * Like bool_pref, but with additional handling code
 *)

let abstract_bool_pref name i s top =
  let v = Textvariable.create_temporary top in
  (* The frame is just to avoid expanding *)
  let f = Frame.create top [] in
  let w = Checkbutton.create f [Text name; Variable v] in
   pack [w][Side Side_Left; Anchor W; Fill Fill_X];
  let p = {
    pref_type = AbstractType(i,s);
    pref_variable = v;
    packed_widget = f;
    pref_name = name;
    resource_name = resource_name name} in
  (* Automatically perform the preference change when you trigger the button *)
  Checkbutton.configure w [Command (fun () -> set_pref p)];
  p
(*e: function Prefs.abstract_bool_pref *)

(*s: function Prefs.abstract_string_pref *)
(*
 * Like string_pref, but with additional handling code
 *)
let abstract_string_pref name i s top =
  let v = Textvariable.create_temporary top in
  let f,e = Frx_entry.new_labelm_entry top name v in
  let p ={
    pref_type = AbstractType(i,s);
    pref_variable = v;
    packed_widget = f;
    pref_name = name;
    resource_name = resource_name name} in
  (* see above *)
  let rec el () = Textvariable.handle v (fun () -> set_pref p; el()) in
  el(); p
(*e: function Prefs.abstract_string_pref *)


(*s: function Prefs.option_handlers *)
(*
 * Utility for option_pref
 *)

let option_handlers mapping read_internal write_internal =
  let rev_mapping = List.map (fun (x,v) -> (v,x)) mapping in
  let init v =
    let current = read_internal() in
    let s =
      try
    List.assoc current mapping
      with
    Not_found ->
      match mapping with
        [] -> "undefined"
      | (x,v)::l -> v
    in
    Textvariable.set v s
  and set v =
    let current = Textvariable.get v in
    let value =
      try
    List.assoc current rev_mapping
      with
    Not_found ->
      match mapping with
        [] -> assert false
      | (x,v)::l -> x
    in
    write_internal value
  in
  init, set, List.map snd mapping
(*e: function Prefs.option_handlers *)


(*
 * Loading and saving preferences from a resource file
 *)

module PrefMap = Map.Make(struct type t = string let compare = compare end)

(*s: function Prefs.load_file *)
let load_file f =
  (* It just loads the file as resource *)
  try
    Tkresource.readfile f Interactive
  with
    Protocol.TkError _ -> 
      failwith (I18n.sprintf "Can't open preference file: %s" f)
(*e: function Prefs.load_file *)

(*s: function Prefs.save_file *)
let save_file prefmaps f =
  let delimiter = "!!! Don't edit below this line !!!" in
  try
    (* create $HOME/.mmm (by default) silently *)
    let prefdir = Filename.dirname f in 
    if not (Sys.file_exists prefdir) then Munix.digdir prefdir 0o755;
    let oc = open_out (f ^ ".tmp") in
    try
      let ic = open_in f in
      try
    while true do
      let l = input_line ic in
      if l = delimiter then raise End_of_file
      else output_string oc (l ^ "\n")
    done
      with
    End_of_file -> 
      close_in ic; 
      raise End_of_file
    with
      Sys_error _ 
    | End_of_file ->
  	(* the delimiter is found, no delimiter in the file
  	   or no pref file is found *)
    output_string oc (delimiter ^ "\n");
    List.iter (
    PrefMap.iter (fun name data ->
      output_char oc '*'; output_string oc name; output_char oc ':'; 
      output_string oc data; output_char oc '\n'))
      prefmaps;
    close_out oc;
    Unix.rename (f ^ ".tmp") f
  with
    Sys_error s ->
      pref_error (I18n.sprintf "Can't open preference file: %s (%s)" f s)
(*e: function Prefs.save_file *)
      
(*s: enum Prefs.pref_family (./gui/prefs.ml) *)
(* Builds a family of preferences *)
type pref_family =
  {family_widget: Widget.widget;    (* the main widget for this family *)
   family_init : unit -> unit;	    (* init the display from memory *)
   family_save : unit -> string PrefMap.t; (* return current bindings *)
   family_load : unit -> unit;  (* loads from persistent storage *)
   family_title : string;
  }
(*e: enum Prefs.pref_family (./gui/prefs.ml) *)

(*s: function Prefs.family *)
(* Computing a family from the predefined preference types *)
let family top title preff =
  let f = 
    Frame.create_named top (Mstring.gensym "family")
      [Relief Sunken; BorderWidth (Pixels 1)] in
  (* create the widgets *)
  let prefs = List.map (fun p -> p f) preff in
  (* define the functions for the family *) 
  let init _ = List.iter init_pref prefs
  and load () = List.iter load_pref prefs
  and save () = 
    List.fold_right 
      (fun pref map -> save_pref (fun k v -> PrefMap.add k v map) pref)
      prefs
      PrefMap.empty
  in
  (* initialize the text variables *)
  init();
  (* wrapping stuff *)
  let t = Label.create f [Text title] in
  pack [t][];
  pack (List.map (fun p -> p.packed_widget) prefs) 
       [Fill Fill_X; Expand true; Anchor W];
  {family_widget = f; family_init = init;
   family_load = load; family_save = save;
   family_title = title}
(*e: function Prefs.family *)


(*s: function Prefs.init *)
(* This is the startup *)

let rec init filename status interactive mute =
  let top = Toplevel.create_named Widget.default_toplevel "prefs" 
      	       	  [Class "MMMPrefs"] in
   Wm.title_set top (I18n.sprintf "MMM Preferences");
   Wm.withdraw top;
   status := Some top;
   bind top [[], Destroy] 
     (BindSet ([Ev_Widget],
           (fun ei -> if ei.ev_Widget = top then status := None)));

  let preffilev = Textvariable.create_temporary top in

  (* The menu bar *)
  let mbar = Frame.create_named top "menubar" [] in
  let file = 
      Menubutton.create_named mbar "file"
      	 [Text (I18n.sprintf "File"); UnderlinedChar 0]
  in
  pack [file][Side Side_Left];
  pack [mbar][Side Side_Top; Anchor W; Fill Fill_X];
  (* The window *)
  let hgroup = Frame.create_named top "panels" [] in
  (* section choice *)
  let sectionf = Frame.create_named hgroup "sections" [] in
  let buttonsf = Frame.create_named top "buttons" [] in
  pack [sectionf] [Side Side_Left; Fill Fill_Y];
  pack [hgroup] [Side Side_Top; Fill Fill_Both; Expand true];
  pack [buttonsf] [Side Side_Bottom];

  Textvariable.set preffilev !filename;	(* for the file selector *)

  (* We must load the file because some elements of the panel depend
     on resources defined in this file *)
  begin 
    try load_file !filename
    with Failure s -> pref_error s
  end;


  (* Then we must do the mute stuff *)
    List.iter (fun f -> f()) mute;

  (* Then we can build the families *)
  let families = List.map (fun f -> f hgroup) interactive in

  (* Then we do the interactive stuff *)
  List.iter (fun f -> f.family_load ()) families;

  let reset () =
    destroy top; status := None;
    init filename status interactive mute
  in

  (* select a preference file to load *)
  let rec load () =
    Fileselect.f (I18n.sprintf "Load a preference file")
      (function [] -> ()
              | [s] -> 
         (* we must restart the panel, because resources
            may affect the displayed menus *)
          if Sys.file_exists s then begin
            destroy top;
            filename := s;
            init filename status interactive mute
          end
          else
            pref_error (I18n.sprintf "%s : no such preference file"
                             s)
              | l -> raise (Failure "multiple selection"))
      (Filename.concat (Filename.dirname (Textvariable.get preffilev))
      	"*")
      (Filename.basename (Textvariable.get preffilev))
      false
      false

  (* select a new preference file to save in *)
  and save_as () =
    Fileselect.f (I18n.sprintf "Save preferences to file")
      (function 
      [] -> ()
        | [s] ->
        Textvariable.set preffilev s;
        filename := s;
  	    begin
              try 
        save_file (List.map (fun f -> f.family_save()) families) s;
        dismiss()
  	      with Failure s -> pref_error s
            end
        | l -> raise (Failure "multiple selection"))
      (Filename.concat (Filename.dirname (Textvariable.get preffilev))
      	"*")
      (Filename.basename (Textvariable.get preffilev))
      false
      false

  (* save in the last defined preference file *)
  and save () =
    try
      save_file (List.map (fun f -> f.family_save()) families) 
            (Textvariable.get preffilev);
      dismiss()
    with
      Failure s -> pref_error s

  and dismiss() = 
    Wm.withdraw top

  in

  (* Fill in the menu *)
  let mfile = Menu.create_named file "filemenu" [] in
    Menu.add_command mfile 
      	[Label (I18n.sprintf "Load"); Command load; UnderlinedChar 0];
    Menu.add_command mfile 
      	[Label (I18n.sprintf "Save"); Command save; UnderlinedChar 0];
    Menu.add_command mfile 
      	[Label (I18n.sprintf "Save As"); Command save_as; UnderlinedChar 0];
    Menu.add_command mfile 
        [Label (I18n.sprintf "Dismiss"); Command dismiss; UnderlinedChar 0];
    Menubutton.configure file [Menu mfile];

  (* Define the buttons *)
    let saveb = Button.create_named buttonsf "save"
      [Text (I18n.sprintf "Save"); Command save]
    and resetb = Button.create_named buttonsf "reset"
      [Text (I18n.sprintf "Reset"); Command reset]
    and dismissb = Button.create_named buttonsf "dismiss"
      [Text (I18n.sprintf "Dismiss"); Command dismiss]
    in
    pack [saveb;resetb;dismissb][Side Side_Left; PadX (Pixels 20)];


  let current = ref (List.hd families) in

  let set_current f =
    Pack.forget [!current.family_widget];
    f.family_init();
    pack [f.family_widget]
         [Side Side_Top; Fill Fill_Both; Expand true];
    current := f in

  let sectionv = Textvariable.create_temporary sectionf in
  let selectors =
    List.map (fun f ->
      Radiobutton.create sectionf [
        Variable sectionv; Text f.family_title; Value f.family_title;
        Command (fun () -> set_current f)]
      )
    families;
  in
  pack selectors [Anchor W];

  Textvariable.set sectionv !current.family_title;

  set_current (List.hd families)
(*e: function Prefs.init *)


(*s: function Prefs.define *)
(* Define a preference panel *)
let define filename interactive mute =
  let inited = ref None 
  and current_file = ref filename in
  (function () -> 
    match !inited with
      Some w -> Wm.deiconify w
    | None -> (* we have been destroyed ! *)
    init current_file inited interactive mute)
(*e: function Prefs.define *)
(*e: ./gui/prefs.ml *)

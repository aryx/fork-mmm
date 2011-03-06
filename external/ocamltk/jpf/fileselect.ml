(* file selection box *)
open Unix
open Str
open Filename

open Tk
open Widget

exception Not_selected

(********************************************************** Search directory *)
(* Default is curdir *)
let global_dir = ref (getcwd ())

(***************************************************** Some widgets creation *)

(* from frx_listbox.ml *)
let scroll_link sb lb =
  Listbox.configure lb 
      	[YScrollCommand (Scrollbar.set sb)];
  Scrollbar.configure sb 
        [ScrollCommand (Listbox.yview lb)]

(* focus when enter binding *)
let bind_enter_focus w = 
  bind w [[], Enter] (BindSet ([], fun _ -> Focus.set w));;

let myentry_create p opts =
  let w = Entry.create p ([Relief Sunken] @ opts) in
  bind_enter_focus w; w

(*
let mylabel_create p opts =
  Label.create p ([Font "variable"] @ opts)
*)

(*
let mybutton_create p opts =
  Button.create p ([Font "variable"] @ opts)
*)

(*
let myscrollbar_create p opts =
  Scrollbar.create p ([Relief Sunken; Width (Pixels 11)] @ opts)
*)

(***************************************************************** Path name *)

(* find directory name which doesn't contain "?*[" *)
let dirget = regexp "^\([^\*?[]*/\)\(.*\)"

let parse_filter src = 
  (* replace // by / *)
  let s = global_replace (regexp "/+") "/" src in
  (* replace /./ by / *)
  let s = global_replace (regexp "/\./") "/" s in
  (* replace ????/../ by "" *)
  let s = global_replace 
    (regexp "\([^/]\|[^\./][^/]\|[^/][^\./]\|[^/][^/]+\)/\.\./") "" s in
  (* replace ????/..$ by "" *)
  let s = global_replace 
    (regexp "\([^/]\|[^\./][^/]\|[^/][^\./]\|[^/][^/]+\)/\.\.$") "" s in
  (* replace ^/../../ by / *)
  let s = global_replace (regexp "^\(/\.\.\)+/") "/" s in
  if string_match dirget s 0 then 
    let dirs = matched_group 1 s
    and ptrn = matched_group 2 s
    in
      dirs, ptrn
  else "", s
 
let ls dir pattern =
  Shell.subshell ("cd " ^ dir ^ ";/bin/ls -ad " ^ pattern ^" 2>/dev/null")

(*************************************************************** File System *)

let get_files_in_directory dir = 
  let dirh = opendir dir in
  let rec get_them () =
   try 
    let x = readdir dirh in  (* no let cause Out of memory *)
      x::(get_them ())
   with
      End_of_file -> closedir dirh; [] 
  in
    Sort.list (<) (get_them ())
      
let rec get_directories_in_files path = function
    [] -> []
  | x::xs -> 
      if try (stat (path ^ x)).st_kind = S_DIR with _ -> false then
      	x::(get_directories_in_files path xs)
      else get_directories_in_files path xs

let remove_directories dirname = 
  let rec remove = function
    [] -> []
  | x :: xs ->
    if try (stat (dirname ^ x)).st_kind = S_DIR with _ -> true then 
      remove xs
    else  
      x :: (remove xs)
  in remove

(************************* a nice interface to listbox - from frx_listbox.ml *)

let add_completion lb action =
  let prefx = ref ""		  (* current match prefix *)
  and maxi = ref 0                (* maximum index (doesn'y matter actually) *)
  and current = ref 0              (* current position *)
  and lastevent = ref 0 in

  let rec move_forward () =
    if Listbox.get lb (Number !current) < !prefx then
      if !current < !maxi then begin incr current; move_forward() end

  and recenter () =
    let element = Number !current in
     (* Clean the selection *)
     Listbox.selection_clear lb (Number 0) End;
     (* Set it to our unique element *)
     Listbox.selection_set lb element element;
     (* Activate it, to keep consistent with Up/Down.
        You have to be in Extended or Browse mode *)
     Listbox.activate lb element;
     Listbox.selection_anchor lb element;
     Listbox.see lb element in

  let complete time s =
    if time - !lastevent < 500 then   (* sorry, hard coded limit *)
      prefx := !prefx ^ s
    else begin (* reset *)
      current := 0;
      prefx := s
    end;
    lastevent := time;
    move_forward();
    recenter() in


  bind lb [[], KeyPress] 
      (BindSet([Ev_Char; Ev_Time], 
      	  (function ev -> 
	     (* consider only keys producing characters. The callback is called
	      * even if you press Shift.
              *)
      	     if ev.ev_Char <> "" then complete ev.ev_Time ev.ev_Char)));
  (* Key specific bindings override KeyPress *)
  bind lb [[], KeyPressDetail "Return"] (BindSet([], action));
  (* Finally, we have to set focus, otherwise events dont get through *)
  Focus.set lb;
  recenter()   (* so that first item is selected *);
  (* returns init_completion function *)
  (fun lb ->
    prefx := "";
    maxi := Listbox.size lb - 1;
    current := 0)

(****************************************************************** Creation *)

let f title proc deffilter deffile multi sync =
  (* Ah ! Now I regret about the names of the widgets... *)

  let current_pattern = ref ""
  and current_dir = ref "" in
  
  (* init_completions *)
  let filter_init_completion = ref (fun _ -> ())
  and directory_init_completion = ref (fun _ -> ()) in
  
  let tl = Toplevel.create default_toplevel [Class "Fileselect"] in
  Focus.set tl;
  Wm.title_set tl title;

  let filter_var = Textvariable.create_temporary tl
  and selection_var = Textvariable.create_temporary tl
  and sync_var = Textvariable.create_temporary tl in

  let frm' = Frame.create tl [BorderWidth (Pixels 1); Relief Raised] in
    let frm = Frame.create frm' [BorderWidth (Pixels 8)] in
    let fl = Label.create frm [Text "Filter"] in
    let df = Frame.create frm [] in
      let dfl = Frame.create df [] in
        let dfll = Label.create dfl [Text "Directories"] in
	let dflf = Frame.create dfl [] in
          let directory_listbox = Listbox.create dflf [Relief Sunken] in
          let directory_scrollbar = Scrollbar.create dflf [] in
            scroll_link directory_scrollbar directory_listbox; 
      let dfr = Frame.create df [] in
        let dfrl = Label.create dfr [Text "Files"] in
	let dfrf = Frame.create dfr [] in
	  let filter_listbox = Listbox.create dfrf [Relief Sunken] in
	  let filter_scrollbar = Scrollbar.create dfrf [] in
	    scroll_link filter_scrollbar filter_listbox;
    let sl = Label.create frm [Text "Selection"] in
    let filter_entry = myentry_create frm [TextVariable filter_var] in
    let selection_entry = myentry_create frm [TextVariable selection_var] in
  let cfrm' = Frame.create tl [BorderWidth (Pixels 1); Relief Raised] in
    let cfrm = Frame.create cfrm' [BorderWidth (Pixels 8)] in
    let dumf = Frame.create cfrm [] in
    let dumf2 = Frame.create cfrm [] in

  let configure filter =
    (* OLDER let curdir = getcwd () in *)
(* Printf.eprintf "CURDIR %s\n" curdir; *)
    let filter =
      if string_match  (regexp "^/.*") filter 0 then filter
      else 
      	if filter = "" then !global_dir ^ "/*"
      	else !global_dir ^ "/" ^ filter in
(* Printf.eprintf "FILTER %s\n" filter; *)
    let dirname, patternname = parse_filter filter in
(* Printf.eprintf "DIRNAME %s PATTERNNAME %s\n" dirname patternname; *)
      current_dir := dirname;
      global_dir := dirname;
    let patternname = if patternname = "" then "*" else patternname in
      current_pattern := patternname;
    let filter = dirname ^ patternname in
(* Printf.eprintf "FILTER : %s\n\n" filter; *)
(* flush Pervasives.stderr; *)
    try
      let directories = get_directories_in_files dirname 
  	    (get_files_in_directory dirname) in
      (* get matched file by subshell call. *)
      let matched_files = remove_directories dirname (ls dirname patternname) 
      in
  	Textvariable.set filter_var filter;
  	Textvariable.set selection_var (dirname ^ deffile); 
  	Listbox.delete directory_listbox (Number 0) End;
  	Listbox.insert directory_listbox End directories;
  	Listbox.delete filter_listbox (Number 0) End;
  	Listbox.insert filter_listbox End matched_files;
  	!directory_init_completion directory_listbox;
  	!filter_init_completion filter_listbox
    with
      Unix_error (ENOENT,_,_) -> 
      	(* Directory is not found (maybe) *)
	Bell.ring ()
  in
  
  let selected_files = ref [] in (* used for synchronous mode *)
  let activate l () =
    Grab.release tl;
    destroy tl;
    if sync then 
      begin
        selected_files := l;
      	Textvariable.set sync_var "1";
	break ()
      end
    else 
      begin
	proc l; 
	break ()
      end 
  in
  
  (* and buttons *)
    let okb = Button.create_named cfrm "ok" [Text "OK";
      Command (fun () -> 
      	let files = 
      	  List.map (fun x -> !current_dir ^ (Listbox.get filter_listbox x))
      	    (Listbox.curselection filter_listbox) in
        let files = if files = [] then [Textvariable.get selection_var] 
                                  else files in
      	activate files () )] in
    let flb = Button.create_named cfrm "filter" [Text "Filter";
      Command (fun () -> configure (Textvariable.get filter_var))] in
    let ccb = Button.create_named cfrm "cancel" [Text "Cancel";
      Command (fun () -> activate [] ())] in

  (* binding *)
  bind selection_entry [[], KeyPressDetail "Return"] (BindSetBreakable ([], 
    fun _ -> activate [Textvariable.get selection_var] () )); 
  bind filter_entry [[], KeyPressDetail "Return"] (BindSet ([], 
    fun _ -> configure (Textvariable.get filter_var) ));
  
  let action _ = 
      let files = 
      	List.map (fun x -> !current_dir ^ (Listbox.get filter_listbox x)) 
      	  (Listbox.curselection filter_listbox)
      in
      	activate files () 
  in
  bind filter_listbox [[Double], ButtonPressDetail 1] 
    (BindSetBreakable ([], action));
  if multi then Listbox.configure filter_listbox [SelectMode Multiple];
  filter_init_completion := add_completion filter_listbox action;

  let action _ =
    try
      configure (!current_dir ^ ((function
      	  [x] -> Listbox.get directory_listbox x
      	| _ -> (* you must choose at least one directory. *)
      	    Bell.ring (); raise Not_selected)
       (Listbox.curselection directory_listbox)) ^ "/" ^ !current_pattern) 
    with _ -> () in
  bind directory_listbox [[Double], ButtonPressDetail 1]
    (BindSetBreakable ([], action));
  Listbox.configure directory_listbox [SelectMode Browse];
  directory_init_completion := add_completion directory_listbox action;

    pack [frm'; frm] [Fill Fill_X];
    (* filter *)
    pack [fl] [Side Side_Top; Anchor W];
    pack [filter_entry] [Side Side_Top; Fill Fill_X];
    (* directory + files *)
    pack [df] [Side Side_Top; Fill Fill_X; IPadX (Pixels 8)];
    (* directory *)
    pack [dfl] [Side Side_Left];
    pack [dfll] [Side Side_Top; Anchor W];
    pack [dflf] [Side Side_Top];
    pack [directory_listbox; directory_scrollbar] [Side Side_Left; Fill Fill_Y];
    (* files *)
    pack [dfr] [Side Side_Right];
    pack [dfrl] [Side Side_Top; Anchor W];
    pack [dfrf] [Side Side_Top];
    pack [filter_listbox; filter_scrollbar] [Side Side_Left; Fill Fill_Y]; 
    (* selection *)
    pack [sl] [Side Side_Top; Anchor W];
    pack [selection_entry] [Side Side_Top; Fill Fill_X];

    (* create OK, Filter and Cancel buttons *)
    pack [cfrm'] [Fill Fill_X];
    pack [cfrm] [Fill Fill_X];
    pack [okb] [Side Side_Left];
    pack [dumf] [Side Side_Left; Expand true];
    pack [flb] [Side Side_Left];
    pack [dumf2] [Side Side_Left; Expand true];
    pack [ccb] [Side Side_Left];

    configure deffilter;

    Tkwait.visibility tl;
    Grab.set tl;

    if sync then
      begin
        Tkwait.variable sync_var;
	proc !selected_files
      end;
    ()

(*s: ./gui/mmm.ml *)
(* The navigation window *)
open Printf
open Unix
open Tk
open Mstring
open Url
open Hyper
open Www
open History
open Document
open Viewers
open Nav

(*s: constant Mmm.hotlist *)
(* Preference settings *)
let hotlist = ref ""
(*e: constant Mmm.hotlist *)
(*s: constant Mmm.helpurl *)
let helpurl = ref (Lexurl.make (Version.helpurl (Lang.lang ())))
(*e: constant Mmm.helpurl *)
(*s: constant Mmm.initial_page *)
let initial_page = ref None
(*e: constant Mmm.initial_page *)
(*s: constant Mmm.initial_geom *)
let initial_geom = ref None
(*e: constant Mmm.initial_geom *)

(*s: constant Mmm.home *)
let home =
  try
    Sys.getenv "HOME"
  with
  | Not_found -> 
      prerr_endline "Please set the HOME environment variable.";
      exit (-1)
(*e: constant Mmm.home *)
      

(*s: function Mmm.user_file *)
let user_file name =
  Filename.concat (Filename.concat home ".mmm") name
(*e: function Mmm.user_file *)

(*s: constant Mmm.preferences *)
(* placeholder for preference panel *)
let preferences = ref (fun () -> ())
(*e: constant Mmm.preferences *)

(*s: constant Mmm.container_frame *)
(* Tachymeter support
 * [container_frame] is the parent frame for displaying a tachymeter
 * It's initialized only after the first navigator window is created.
 * [tachy_maker] contains the current tachymeter creation function.
 * [change_tachy] changes the current tachymeter. It has immediate
 * effect if the first navigator window is already available. Otherwise,
 * it will take effect at creation time, using [start_tachy].
 *)
let container_frame = ref None
(*e: constant Mmm.container_frame *)
(*s: constant Mmm.tachy_maker *)
let tachy_maker = ref About.create_tachy
(*e: constant Mmm.tachy_maker *)

(*s: function Mmm.change_tachy *)
let change_tachy (t : Widget.widget -> Low.tachymeter) = 
  !Low.cur_tachy#quit;
  tachy_maker := t;
  begin match !container_frame with
    Some f -> 
      List.iter Tk.destroy (Winfo.children f);
      Low.cur_tachy := t f
  | None -> ()
  end
(*e: function Mmm.change_tachy *)

(*s: function Mmm.start_tachy *)
let start_tachy () = 
  begin match !container_frame with
    Some f -> 
      Low.cur_tachy := !tachy_maker f
  | None -> ()
  end
(*e: function Mmm.start_tachy *)

(* Switching current viewers in the browser *)
(*s: function Mmm.undisplay *)
let undisplay di = 
  if Winfo.exists di#di_widget 
  then Pack.forget [di#di_widget]
(*e: function Mmm.undisplay *)
(*s: function Mmm.display *)
let display di = 
  if Winfo.exists di#di_widget
  then pack [di#di_widget][Fill Fill_Both; Expand true]
  else !Error.default#f "fatal error: window was destroyed";

  let tl = Winfo.toplevel di#di_widget in
  let title = I18n.sprintf "MMM Browser@%s" di#di_title in
  if Widget.known_class tl = "toplevel" 
  then begin 
    Wm.title_set tl title; 
    Wm.iconname_set tl title
  end
(*e: function Mmm.display *)

(*s: function Mmm.quit *)
let quit confirm =
  if confirm then
    match Frx_dialog.f Widget.default_toplevel (gensym "quit")
      (I18n.sprintf "Confirm") 
      (I18n.sprintf "Do you really want to quit ?")
       (Predefined "question") 0 
       [I18n.sprintf "Yep"; I18n.sprintf "Nope"] 
    with
    |  0 -> destroy Widget.default_toplevel
    | _ -> ()
  else destroy Widget.default_toplevel
(*e: function Mmm.quit *)

(*s: constant Mmm.user_menus *)
(* User defined menus *)
let user_menus = ref []
(*e: constant Mmm.user_menus *)
(*s: function Mmm.add_user_menu *)
let add_user_menu entry f = 
  user_menus := (entry,(fun x -> try f x with _ ->())) :: !user_menus;
  Frx_synth.broadcast "user_menu"
(*e: function Mmm.add_user_menu *)

(*s: constant Mmm.navigators *)
(*
 * A navigator window
 *)
let navigators = ref 0
(*e: constant Mmm.navigators *)

(*s: function Mmm.navigator *)
let rec navigator has_tachy initial_url =
  (*s: [[Mmm.navigator()]] new navigator hook *)
  incr navigators;
  (*e: [[Mmm.navigator()]] new navigator hook *)

  (* The first navigator is named, so we can put special information in
   * window manager configurations, such as sticky 
   *)
  let top = 
    if has_tachy 
    then Toplevel.create_named Widget.default_toplevel "mmm" [Class "MMM"]
    (*s: [[Mmm.navigator()]] if not main window, create default toplevel *)
    else Toplevel.create Widget.default_toplevel [Class "MMM"]
    (*e: [[Mmm.navigator()]] if not main window, create default toplevel *)
  in
  (*s: [[Mmm.navigator()]] locals *)
  let entryv = Textvariable.create_temporary top in
  (*x: [[Mmm.navigator()]] locals *)
  let current_di = ref None in
  (*x: [[Mmm.navigator()]] locals *)
  let update_vhistory = ref (fun () -> ()) (* duh *) in
  (*e: [[Mmm.navigator()]] locals *)

  Wm.title_set top (I18n.sprintf "MMM Browser");
  (*s: [[Mmm.navigator()]] setup top packing *)
  (* the size of the navigator MUST NOT depend on what is displayed inside *)
  (* Instead, we rely on defaults for class MMM, *MMM.Width, *MMM.Height   *)
  Pack.propagate_set top false;
  (*e: [[Mmm.navigator()]] setup top packing *)

  (* protect all the other initialisations *)
  try 
    (*s: [[Mmm.navigator()]] inside try *)
    (* The frame in which a viewer might want to display *)
    let viewer_frame = Frame.create_named top "viewer" [] in

    (*s: [[Mmm.navigator()]] locals before nav setting *)
    (*s: local Mmm.navigator.hist *)
    let initial_did = 
      { document_url = initial_url; 
        document_stamp = Document.no_stamp
      } 
    in
    let hist = History.create initial_did in
    (*e: local Mmm.navigator.hist *)
    (*s: local function Mmm.navigator.show_current *)
    (* Change view, independantly of history manip *)
    let show_current di frag =
      di#di_touch;
      (match !current_di with
      | None -> display di
      | Some olddi -> 
         if olddi == di 
         then () 
         else begin
           undisplay olddi;
           display di
         end
      );
      current_di := Some di;
      (* bogus if two views with fragment on the same pending document *)
      di#di_fragment frag;
      (* Bof *)
      Textvariable.set entryv (Url.string_of hist.h_current.h_did.document_url)
    in
    (*e: local function Mmm.navigator.show_current *)
    (*s: local function Mmm.navigator.add_hist *)
    let add_hist did frag =
      History.add hist did frag;
      !update_vhistory ()
    in
    (*e: local function Mmm.navigator.add_hist *)
    (*s: local object Mmm.navigator.error *)
    let error = new Tk_error.t top in
    (*e: local object Mmm.navigator.error *)
    (*s: local Mmm.navigator.loggingv *)
    let loggingv = Textvariable.create_temporary top in
    (*e: local Mmm.navigator.loggingv *)
    (*s: local Mmm.navigator.actives *)
    let actives = Hashtbl.create 37 in
    (*e: local Mmm.navigator.actives *)
    (*e: [[Mmm.navigator()]] locals before nav setting *)
    let nav = { 
      (*s: [[Mmm.navigator()]] set nav fields *)
      nav_viewer_frame = viewer_frame;
      (*x: [[Mmm.navigator()]] set nav fields *)
      nav_id = hist.h_key;
      (*x: [[Mmm.navigator()]] set nav fields *)
      nav_new = (fun link ->
           try
             let wwwr = Plink.make link in
             navigator false wwwr.www_url |> ignore; ()
           with Invalid_link msg -> 
            error#f (I18n.sprintf "Invalid link")
      );
      (*x: [[Mmm.navigator()]] set nav fields *)
      nav_add_active = Hashtbl.add actives;
      nav_rem_active = Hashtbl.remove actives;
      (*x: [[Mmm.navigator()]] set nav fields *)
      nav_show_current = show_current;
      (*x: [[Mmm.navigator()]] set nav fields *)
      nav_add_hist = add_hist;
      (*x: [[Mmm.navigator()]] set nav fields *)
      nav_log = (fun s -> Textvariable.set loggingv s);
      (*x: [[Mmm.navigator()]] set nav fields *)
      nav_error = error;
      (*e: [[Mmm.navigator()]] set nav fields *)
    }
    in

    (* The navigation functions *)
    (*s: [[Mmm.navigator()]] navigation functions *)
    (*  The cache may have been cleared, so the document may be lost.
     *  historygoto implements the proper logic for this, taking care
     *  of non-unique documents.
     *)
    let back () = 
      match History.back hist with
       | None -> ()
       | Some (did, frag) -> 
           if not (historygoto nav did frag true) 
           then History.forward hist |> ignore
    in
    let forward () =
      match History.forward hist with
      | None -> ()
      | Some (did, frag) -> 
          if not (historygoto nav did frag true) 
          then History.back hist |> ignore
    in
    let reload () =
      let did = hist.h_current.h_did in
      let frag = hist.h_current.h_fragment in
      if did.document_stamp = no_stamp then begin
        (* kill both in cache and in gcache *)
        Cache.kill did; 
        Gcache.remove hist.h_key did;
        historygoto nav did frag false |> ignore
      end
        else
          error#f (I18n.sprintf "Document cannot be reloaded from its url\n(probably a POST request)")
    in

    let update nocache =
      let did = hist.h_current.h_did in
      if did.document_stamp = no_stamp then
        Nav.update nav did nocache
      else (* POST result *)
        error#f (I18n.sprintf "Can't update document\n(probably a POST request)")
    in
    (*e: [[Mmm.navigator()]] navigation functions *)
    (* A bunch of other functions *)
    (*s: [[Mmm.navigator()]] new window functions *)
    let new_window () =
         navigator false hist.h_current.h_did.document_url |> ignore
    in
    let new_window_initial () =
         navigator false initial_url |> ignore
    in
    let new_window_sel () =
      try 
         let url = Selection.get [] in
         navigator false (Lexurl.make url) |> ignore
      with _ -> navigator false initial_url |> ignore
    in
    (*e: [[Mmm.navigator()]] new window functions *)
    (*s: [[Mmm.navigator()]] other functions *)
    let abort () =
      actives |> Hashtbl.iter (fun url abort -> abort());
      Hashtbl.clear actives;
      match !current_di with
      | None -> ()
      | Some di -> di#di_abort
    in
    let open_sel () =
      try 
        let url = Selection.get [] in
        absolutegoto nav url
      with _ -> ()
    in
    let open_file () =
      Fileselect.f (I18n.sprintf "Open File") (function
        | [] -> ()
        | [s] -> 
            let path = Msys.tilde_subst s in
            absolutegoto nav ("file://localhost/"^path)
        | l -> raise (Failure "multiple selection")
       )
         "*" 
         ""
         false
         false
    in
    let save () = 
     Save.document hist.h_current.h_did None 
    in
    let print () = 
      Save.document hist.h_current.h_did 
         (Some (sprintf "|%s" !Save.print_command))
    in
    let close () =
      if !navigators = 1 
      then quit true
      else destroy top
    in
    let really_quit () = 
      quit false
    in
    let gohome () = 
      absolutegoto nav !Mmmprefs.home
    in
    let redisplay () =
      match !current_di with
      | None -> ()
      | Some di -> di#di_redisplay
    in
    let add_to_hotlist () =
      match !current_di with
      | None -> ()
      | Some di -> 
          Hotlist.f (Url.string_of hist.h_current.h_did.document_url)
                      di#di_title
    in
    let load_images () =
      match !current_di with
      | None -> ()
      | Some di -> di#di_load_images
    in
    let view_source () =     
      match !current_di with
      | None -> ()
      | Some di -> di#di_source
    in
    (*e: [[Mmm.navigator()]] other functions *)

    (* Short cuts *)
    (*s: [[Mmm.navigator()]] short cuts *)
    (* All the available shortcuts functions and their short cut keys. *)
    (* If you put a new function with its short cut key here, then *)
    (* Short cut string will be displayed automatically, when these *)
    (* functions are added as menu elements. *)

    (* Sorry, we use function equality, so we cannot use lambdas in the list *)
    let update_true = fun () -> update true in

    (* The shortcuts and the default settings *)
    let all_short_cuts = [
      (* function    resource name      default key sequence *)
      About.f,     "About",           [[], KeyPressDetail "F1"]; 
      new_window,    "NewWindow",       [[Alt], KeyPressDetail "n"];
      open_sel,      "OpenSelection",   [[Alt], KeyPressDetail "y"];
      open_file,     "OpenFile",        [[Alt], KeyPressDetail "o"];
      save,          "Save",            [[Alt], KeyPressDetail "s"];
      print,         "Print",           [];
      !preferences,  "Preference",      [[Alt], KeyPressDetail "p"];
      close,         "Close",           [[Alt], KeyPressDetail "c"];
      really_quit,   "Quit",            [[Alt], KeyPressDetail "q"];

      gohome,        "Home",            [];
      back,          "Back",            [[Alt], KeyPressDetail "Left"];
      forward,       "Forward",         [[Alt], KeyPressDetail "Right"];
      reload,        "Reload",          [[Alt], KeyPressDetail "r"];
      abort,         "Abort",           [[], KeyPressDetail "Escape"];

      update_true,   "Update",          [[Alt], KeyPressDetail "u"];
      redisplay,     "Redisplay",       [[Control], KeyPressDetail "l"];
      add_to_hotlist, "AddToHotlist",   [[Alt], KeyPressDetail "a"];
      load_images,   "LoadImages",      [[Alt], KeyPressDetail "i"];
      view_source,   "ViewSource",      [[Alt], KeyPressDetail "e"]
    ] 
    in

    (* Real shortcuts information actually used *)
    let my_short_cuts = List.map (fun (f,r,d) ->
      f, Tkresource.event_sequence ("shortcut" ^ r) d) all_short_cuts
    in

    (* we break after each event so that All bindings, such as menu traversal,
     * dont get invoked if we destroyed the window for some reason
     * may be required only for things like reload
     *)
    my_short_cuts |> List.iter (fun (f, eventl) -> 
      if eventl <> [] 
      then bind top eventl (BindSetBreakable ([], fun _ -> f(); break()))
    );
    (*e: [[Mmm.navigator()]] short cuts *)

    (* Invariable part (the rest being the di stuff)
       hgroup: blah and tachymeter
     *)
    let hgroup = Frame.create_named top "hgroup" [] in
    let vgroup = Frame.create_named hgroup "vgroup" [] in (* Menus, open entry *)

    let mbar = Frame.create_named vgroup "menubar" [] in
    (*s: [[Mmm.navigator()]] setup menu *)
    (*s: function Mmm.navigator.configure_menu_elements *)
    let configure_menu_elements menu xs =
      let rec list_assoc_address k = function
        | (k',v)::_ when k == k' -> v
        | _::xs -> list_assoc_address k xs
        |	[] -> raise Not_found
      in
      xs |> List.iter (fun l ->
        let opts = 
         List.fold_right (fun opt st ->
           (match opt with
           | Command f -> 
               begin
                Command f :: 
                  try
                    [ Accelerator (Tkresource.short_event_sequence
                                   (list_assoc_address f my_short_cuts))]
                  with Not_found -> []
               end
           | _ -> [opt])
           @ st
         ) l []
        in
        match opts with
        | [] -> Menu.add_separator menu
        | _  -> Menu.add_command menu opts
     )
    in
    (*e: function Mmm.navigator.configure_menu_elements *)
    (*s: [[Mmm.navigator()]] MMM menu *)
    (* MMM menu *)
    let mmm = Menubutton.create_named mbar "mmm" [Text (I18n.sprintf "MMM")] in
    let mmmm = Menu.create_named mmm "menu" [] in
    Menubutton.configure mmm [Menu mmmm];
    configure_menu_elements mmmm [
      [Label (I18n.sprintf "About")            ; Command About.f];
      [];
      [Label (I18n.sprintf "New Window")       ; Command new_window];
      [Label (I18n.sprintf "Open Selection")   ; Command open_sel];
      [Label (I18n.sprintf "Open File...")     ; Command open_file];
      [Label (I18n.sprintf "Save document...") ; Command save];
      [Label (I18n.sprintf "Print document")   ; Command print];
      [Label (I18n.sprintf "Preferences...")   ; Command !preferences];
      [];
      [Label (I18n.sprintf "Close Window")     ; Command close];
      [];
      [Label (I18n.sprintf "Quit")             ; Command really_quit]
    ];
    (*e: [[Mmm.navigator()]] MMM menu *)
    (*s: [[Mmm.navigator()]] Navigation menu *)
    (* Navigation menu *)
    let navb = Menubutton.create_named mbar "navigate" [Text (I18n.sprintf "Navigate")] in
    let navm = Menu.create_named navb "menu" [] in
    Menubutton.configure navb [Menu navm];
    configure_menu_elements navm [ 
      [Label (I18n.sprintf "Home"); Command gohome];
      [Label (I18n.sprintf "Back"); Command back];
      [Label (I18n.sprintf "Forward"); Command forward];
      []
    ];
    (*e: [[Mmm.navigator()]] Navigation menu *)
    (*s: [[Mmm.navigator()]] History menu *)
    (* The history menu is destroyed and rebuild each time. 
     * Deleting all entries will cause a callback leak since
     *  entries are associated to the menu itself 
     *)
    let history_mindex = Pattern (I18n.sprintf "History") in
    let hmenu = ref (Menu.create_named navm "history" []) in 
    Menu.add_cascade navm [Label (I18n.sprintf "History")];
    update_vhistory := (fun () ->
      destroy !hmenu;
      hmenu := Menu.create_named navm "history" [];
      History.contents hist |> List.iter (fun e ->
        let label = ref (Url.string_of e.h_did.document_url) in
        (match e.h_fragment with
        | None -> ()
        | Some f -> label := !label^"#"^f
        );
        (match e.h_did.document_stamp with
        | 0 -> ()
        | n -> label := !label^"("^string_of_int n^")"
        );
        Menu.add_command !hmenu 
           [Label !label;
            Command (fun () ->
              let cure = hist.h_current in
              History.set_current hist e;
              if not (historygoto nav e.h_did e.h_fragment true)
              then History.set_current hist cure)
            ]
        );
        Menu.configure_cascade navm history_mindex [Menu !hmenu]
    );

    (*e: [[Mmm.navigator()]] History menu *)
    (*s: [[Mmm.navigator()]] Document menu *)
    let docb = Menubutton.create_named mbar "document" [Text (I18n.sprintf "Document")] in
    let docm = Menu.create_named docb "menu" [] in
    Menubutton.configure docb [Menu docm];
    configure_menu_elements docm [	    
      [Label (I18n.sprintf "Abort")          ; Command abort];
      [Label (I18n.sprintf "Reload")         ; Command reload];
      [Label (I18n.sprintf "Update")         ; Command update_true];
      [Label (I18n.sprintf "Redisplay")      ; Command redisplay];
      [Label (I18n.sprintf "Add to hotlist") ; Command add_to_hotlist];
      [Label (I18n.sprintf "Load Images")    ; Command load_images];
      [Label (I18n.sprintf "View Source")    ; Command view_source]
    ];
    (*e: [[Mmm.navigator()]] Document menu *)
    (*s: [[Mmm.navigator()]] Other menu *)
    (* Other stuff *)
    let othersb = Menubutton.create_named mbar "others" [Text (I18n.sprintf "Others")] in
    let othersm = Menu.create_named othersb "menu" [] in
    Menubutton.configure othersb [Menu othersm];
    Menu.add_command othersm
      [Label (I18n.sprintf "Load Authorizations..."); Command Auth.load];
    Menu.add_command othersm
      [Label (I18n.sprintf "Edit Authorizations..."); Command Auth.edit];
    Menu.add_command othersm
      [Label (I18n.sprintf "Save Authorizations..."); Command Auth.save];
    (*e: [[Mmm.navigator()]] Other menu *)
    (*s: [[Mmm.navigator()]] Help menu *)
    (* Help menu *)
    let helpb = Menubutton.create_named mbar "help" [Text (I18n.sprintf "Help")] in
    let helpm = Menu.create_named helpb "menu" [] in
    Menubutton.configure helpb [Menu helpm];
    Menu.add_command helpm
      [Label (I18n.sprintf "Version information");
       Command (fun () -> absolutegoto nav (Version.initurl (Lang.lang ())))];
    Menu.add_command helpm
      [Label (I18n.sprintf "Help on MMM");
       Command (fun () -> navigator false !helpurl |> ignore)];
    Menu.add_command helpm
      [Label (I18n.sprintf "Home Page of MMM");
       Command (fun () -> 
         navigator false (Lexurl.make (Version.home (Lang.lang ()))) |> ignore)];
    (*e: [[Mmm.navigator()]] Help menu *)
    (*s: [[Mmm.navigator()]] User menu *)
    (* User menu, extensible by applets *)
    let userb = Menubutton.create_named mbar "user" [Text (I18n.sprintf "User")] in
    let userm = ref (Menu.create_named userb "menu" []) in
    let reset_user_menu _ =
      destroy !userm;
      userm := Menu.create_named userb "menu" [];
      !user_menus |> List.iter (fun (entry, f) ->
          Menu.add_command !userm 
               [Label entry; 
                Command (fun () -> f (Nav.make_ctx nav hist.h_current.h_did))]
      );
      Menubutton.configure userb [Menu !userm] 
    in
    reset_user_menu();
    Frx_synth.bind userb "user_menu" reset_user_menu;
    (*e: [[Mmm.navigator()]] User menu *)

    pack [mmm; navb; docb; othersb][Side Side_Left];
    pack [helpb; userb] [Side Side_Right];
    (*e: [[Mmm.navigator()]] setup menu *)

    (*s: [[Mmm.navigator()]] setup open url entry *)
    (* URL display and edit *)
    let f,e = Frx_entry.new_label_entry vgroup (I18n.sprintf "Open URL:")
                       (fun url -> Nav.absolutegoto nav url)
    in
    Entry.configure e [TextVariable entryv; TextWidth 40];
    (*e: [[Mmm.navigator()]] setup open url entry *)

    (* Navigation buttons *)
    let fb = Frame.create_named vgroup "buttons" [] in
    (*s: [[Mmm.navigator()]] navigation buttons *)
    let backb = Button.create_named fb 
      "back" [Text (I18n.sprintf "Back"); Command back ] in
    (*x: [[Mmm.navigator()]] navigation buttons *)
    let forwardb = Button.create_named fb 
      "forward" [Text (I18n.sprintf "Forward"); Command forward] in
    (*x: [[Mmm.navigator()]] navigation buttons *)
    let homeb = Button.create_named fb "home"
      [ Text (I18n.sprintf "Home"); Command gohome] in
    (*x: [[Mmm.navigator()]] navigation buttons *)
    let reloadb = Button.create_named fb
      "reload" [Text (I18n.sprintf "Reload"); Command reload] in
    (*x: [[Mmm.navigator()]] navigation buttons *)
    let abortb = Button.create_named fb 
      "abort" [Text (I18n.sprintf "Abort"); Command abort] in
    (*x: [[Mmm.navigator()]] navigation buttons *)
    let loggingb = Label.create_named fb "logging"
      [TextWidth 40; TextVariable loggingv; Anchor W] in
    (*e: [[Mmm.navigator()]] navigation buttons *)

    (*s: [[Mmm.navigator()]] packing part one *)
    pack [mbar][Anchor NW; Side Side_Top; Fill Fill_X];
    pack [backb;homeb;forwardb;reloadb;abortb; loggingb]
           [Side Side_Left; Fill Fill_X];
    pack [f][Fill Fill_X; Expand true; Side Side_Bottom; Anchor SW];
    pack [fb][Fill Fill_X];
    (*e: [[Mmm.navigator()]] packing part one *)
    (* Initial window only *)
    if has_tachy then begin
      (*s: [[Mmm.navigator()]] set geometry if specified *)
      (match !initial_geom with 
       | None -> ()
       | Some g -> 
          Wm.geometry_set top g
       );
      (*e: [[Mmm.navigator()]] set geometry if specified *)
      (*s: [[Mmm.navigator()]] set tachy *)
      (* put this as a function so we can restart it if needed *)
      let rec restart_tachy () =
         (* We must not pass hgroup to tachymeter applets *)
         let fcontainer = Frame.create hgroup [] in
         container_frame := Some fcontainer;
         (* restart it if destroyed *)
         bind fcontainer [[], Destroy]
          (BindSet ([Ev_Widget],
            (fun ei -> 
              if ei.ev_Widget = fcontainer 
                      && Winfo.exists hgroup (* but we're not dead *) then
                restart_tachy())));

        let rw = Winfo.reqwidth fcontainer in
        let rh = Winfo.reqheight fcontainer in
        Wm.minsize_set top rw rh;
        pack [fcontainer][Side Side_Right; Anchor N];
        start_tachy();
        (* Bad hack to do bindings for our own internal tachymeter:
         * others, in applets, can just access these functions from the safe
         * library *)
        if !tachy_maker == About.create_tachy then begin 
          match Winfo.children fcontainer with
          | [c] ->
               bind c (Glevents.get "tachy_new")
                (BindSet ([], (fun _ -> new_window_initial ())));
               bind c (Glevents.get "tachy_sel")
                (BindSet ([], (fun _ -> new_window_sel ())));
          | _ -> ()
        end
      in
      restart_tachy(); (* first initialisation *)
      (* good size for keeping only the tachy *)
      Wm.minsize_set top 80 80;
      (*e: [[Mmm.navigator()]] set tachy *)
    end;
    (*s: [[Mmm.navigator()]] packing part two *)
    (* Pack last to avoid lossage when resizing *)
    pack [vgroup][Fill Fill_X; Expand true; Side Side_Left];
    pack [hgroup][Fill Fill_X];
    pack [viewer_frame][Fill Fill_Both; Expand true];
    (*e: [[Mmm.navigator()]] packing part two *)

    (*s: [[Mmm.navigator()]] handling destroy event *)
    (* We receive this event for each children destroyed because we are
       a toplevel *)
    bind top [[], Destroy] (BindSet ([Ev_Widget], (fun ei -> 
      if ei.ev_Widget = top then begin
        decr navigators;
        Gcache.kill hist.h_key;

        (* we were destroyed by wm *)
        if !navigators = 0 && Winfo.exists Widget.default_toplevel
        then destroy Widget.default_toplevel
      end
    )));
    (*e: [[Mmm.navigator()]] handling destroy event *)
    Tkwait.visibility hgroup;

    (*s: [[Mmm.navigator()]] call update_vhistory *)
    !update_vhistory();
    (*e: [[Mmm.navigator()]] call update_vhistory *)
    (*s: [[Mmm.navigator()]] touch current *)
    (* Yet another timer to avoid flushing displayed documents *)
    let rec touch_current () =
      if Winfo.exists top then begin
        Cache.touch hist.h_current.h_did;
        Timer.set 10000 touch_current
      end 
    in
    touch_current();
    (*e: [[Mmm.navigator()]] touch current *)

    absolutegoto nav (Url.string_of initial_url);
    Some nav
    (*e: [[Mmm.navigator()]] inside try *)

  with e -> 
    !Error.default#f (I18n.sprintf "Can't view initial document: %s\n%s"
                      (Url.string_of initial_url)
                      (Printexc.to_string e));
    if !navigators = 1 then begin
      destroy Widget.default_toplevel;
      raise e
    end 
    (*s: [[Mmm.navigator()]] exn handler, else if multiple navigators *)
    else begin 
      destroy top;
      None
    end
    (*e: [[Mmm.navigator()]] exn handler, else if multiple navigators *)
(*e: function Mmm.navigator *)


(*s: function Mmm.new_window_initial *)
and new_window_initial () =
 navigator false 
   (match !initial_page with | Some u -> u | None -> assert false) |> ignore
(*e: function Mmm.new_window_initial *)

(*s: function Mmm.new_window_set *)
and new_window_sel () =
  try 
    let url = Selection.get [] in
    navigator false (Lexurl.make url) |> ignore
  with _ -> new_window_initial ()
(*e: function Mmm.new_window_set *)


(*s: constant Mmm.main_navigator *)
let main_navigator = ref None
(*e: constant Mmm.main_navigator *)

(*s: function Mmm.initial_navigator *)
let initial_navigator preffile init_url =
  (*s: [[Mmm.initial_navigator()]] set preferences *)
  preferences := Mmmprefs.f preffile;
  !preferences();
  (*e: [[Mmm.initial_navigator()]] set preferences *)
  (*s: [[Mmm.initial_navigator()]] set initial page *)
  initial_page := Some (
     match init_url with
     | None -> Lexurl.make !Mmmprefs.home
     | Some x -> 
         begin
           try Lexurl.make x 
         with _ -> (* If fails, try to use file: *)
           let path = 
             if x.[0] = '/' 
             then x
             else Filename.concat (Unix.getcwd ()) x
           in
           Lexurl.make ("file://localhost" ^ path)
         end
  );
  (*e: [[Mmm.initial_navigator()]] set initial page *)
  main_navigator :=
     navigator true 
       (match !initial_page with Some u -> u | None -> assert false);
  !main_navigator
(*e: function Mmm.initial_navigator *)
(*e: ./gui/mmm.ml *)

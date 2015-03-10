(*s: ./main.ml *)
open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function Main.safe_loop *)
let rec safe_loop() =
  try
    Printexc.print Tk.mainLoop () (* prints and reraises *)
  with
  | Out_of_memory -> raise Out_of_memory
  | Sys.Break -> raise Sys.Break
  | Stack_overflow -> raise Stack_overflow
  | e -> 
      flush Pervasives.stderr; 
      safe_loop()
(*e: function Main.safe_loop *)
       
(*s: function Main.localize *)
let localize file =
  let localized = spf "%s.%s" file !I18n.language in
  if Sys.file_exists localized 
  then localized 
  else file
(*e: function Main.localize *)

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: constant Main.usage_str *)
let usage_str =
  "Usage: meuh <opts> <initial url>"
(*e: constant Main.usage_str *)

(*s: function Main.main *)
let main () =
  (*s: [[Main.main()]] tk backends setup *)
  Error.default                    := new Tk_error.t Widget.default_toplevel;
  Condition.backend                := Tk_condition.backend ();
  Timer_.add_ref                   := (fun a b -> Timer.add a b |> ignore);
  Timer_.set_ref                   := Timer.set;
  Low.update_idletasks_backend     := Tk.update_idletasks;
  Fileevent_.add_fileinput_ref     := Fileevent.add_fileinput;
  Fileevent_.remove_fileinput_ref  := Fileevent.remove_fileinput;
  Fileevent_.add_fileoutput_ref    := Fileevent.add_fileoutput;
  Fileevent_.remove_fileoutput_ref := Fileevent.remove_fileoutput;
  Document.add_log_backend         := Tk_document.add_log;
  Maps.broadcast_backend           := Frx_synth.broadcast;
  Auth.open_passwd_ref             := Frx_req.open_passwd;
  Auth.edit_backend                := Tk_auth.edit;
  Mailto.internal_backend          := Tk_mailto.internal;
  (*e: [[Main.main()]] tk backends setup *)

  (* As always, we must parse argument first, using references... *)
  (*s: [[Main.main()]] locals *)
  let init_urls = ref [] in
  (*x: [[Main.main()]] locals *)
  let display = ref (try Sys.getenv("DISPLAY") with Not_found -> "") in
  (*x: [[Main.main()]] locals *)
  let sufxfile = ref (Mmm.user_file "mime.types") in
  (*x: [[Main.main()]] locals *)
  let preffile = ref (Mmm.user_file "MMM.ad") in
  (*x: [[Main.main()]] locals *)
  let palette = ref None in
  (*x: [[Main.main()]] locals *)
  let clicktofocus = ref false in
  (*x: [[Main.main()]] locals *)
  let accept_external = ref false in
  (*x: [[Main.main()]] locals *)
  let modules = ref true in
  (*e: [[Main.main()]] locals *)
  Arg.parse [
   (*s: [[Main.main()]] command line options *)
   "-d", Arg.String (fun s -> display := s),
   "<foo:0>\t\tDisplay";
   (*x: [[Main.main()]] command line options *)
   "-display", Arg.String (fun s -> display := s),
   "<foo:0>\tDisplay";
   (*x: [[Main.main()]] command line options *)
   "-suffixes", Arg.String (fun s -> sufxfile := s),
   "<file>\tSuffix file";
   (*x: [[Main.main()]] command line options *)
   "-prefs", Arg.String (fun s -> preffile := s),
   "<file>\t\tPreference File";
   (*x: [[Main.main()]] command line options *)
   "-geometry", Arg.String (fun s -> Mmm.initial_geom := Some s),
   "<wxh+x+y>\tInitial geometry for the first navigator";
   (*x: [[Main.main()]] command line options *)
   "-palette", Arg.String (fun s -> palette := Some s),
   "<color>\tTk Palette";
   (*x: [[Main.main()]] command line options *)
   "-clicktofocus", Arg.Unit (fun () -> clicktofocus := true),
   "\tClick to Focus mode (default is Focus Follows Mouse)";
   (*x: [[Main.main()]] command line options *)
   "-helpurl", Arg.String (fun s -> Mmm.helpurl := Lexurl.make s),
   "<url>\tHelp URL";
   (*x: [[Main.main()]] command line options *)
   "-external", Arg.Unit (fun () -> accept_external := true),
   "\t\tAccept remote command (mmm_remote <url>)";
   (*x: [[Main.main()]] command line options *)
   "-proxy", Arg.String (fun s -> Http.proxy := s), 
   "<hostname>\tProxy host";
   (*x: [[Main.main()]] command line options *)
   "-port", Arg.Int (fun i -> Http.proxy_port := i),
   "<port>\t\tProxy port";
   (*x: [[Main.main()]] command line options *)
   "-lang", Arg.String (fun s -> I18n.language := s),
   "<lang>\t\tI18n language";
   (*x: [[Main.main()]] command line options *)
   "-msgfile", Arg.String (fun s -> I18n.message_file := s),
   "<file>\tI18n message file";
   (*x: [[Main.main()]] command line options *)
   "-nomodule", Arg.Unit (fun () -> modules := false),
   "\t\tDon't load initial modules";
   (*e: [[Main.main()]] command line options *)
  ]
    (fun s -> init_urls := s :: !init_urls)
    usage_str
  ;
  (*s: [[Main.main()]] signal handling *)
  Sys.catch_break true;
  (* Avoid SIGPIPE completely, in favor of write() errors *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  (*e: [[Main.main()]] signal handling *)
  (*s: [[Main.main()]] initialisation *)
  (*s: [[Main.main()]] tk initialisation *)
  let top = Tk.openTkDisplayClass !display "mmm" in
  Wm.withdraw top;
  (*x: [[Main.main()]] tk initialisation *)
  if not !clicktofocus 
  then Focus.follows_mouse();
  (*e: [[Main.main()]] tk initialisation *)
  (*s: [[Main.main()]] resource initialisation *)
  (* Default values for navigator window *)
  Resource.add "*MMM.Width" "640" Tk.WidgetDefault;
  Resource.add "*MMM.Height" "480" Tk.WidgetDefault;

  (* Resources *)
  let site_resfile =
    localize (Filename.concat (Filename.dirname Sys.argv.(0)) "MMM.ad") in
  (* Site specific resource file usually in INSTALLDIR=/usr/local/lib/mmm *)
  if Sys.file_exists site_resfile 
  then Tkresource.readfile site_resfile Tk.StartupFile;
  (*x: [[Main.main()]] resource initialisation *)
  begin match !palette with
  | None -> ()
  | Some bg -> try Palette.set_background (Tk.NamedColor bg) with _ -> ()
  end;
  (*e: [[Main.main()]] resource initialisation *)
  (*s: [[Main.main()]] tk libs initialisation *)
  (* Initialisations in frx library : kbd navigation, search 
   * No prerequisite except Tk *)
  Frx_text.init ();
  (* Initialisations in jpf's balloon library *)
  Balloon.init ();
  (* Initialisations in jpf's GIF ANIMATION library *)
  Tkaniminit.f ();
  (*e: [[Main.main()]] tk libs initialisation *)
  (*s: [[Main.main()]] local initialisation *)
  (* Local initialisations *)
  Low.init();                         (* start regular tasks *)
  Cache.init();                       (* builtin document *)
  Auth.init();                        (* start expiration timer *)
  Debug.init();                       (* debugging RPC *)
  (*e: [[Main.main()]] local initialisation *)
  (*s: [[Main.main()]] suffix initialisation *)
  (* Suffix mapping to Content-Type and Content-Encoding *)
  if Sys.file_exists !sufxfile 
  then Http_headers.read_suffix_file !sufxfile;
  (*e: [[Main.main()]] suffix initialisation *)
  (*s: [[Main.main()]] misc initialisation *)
  (* Various stuff for the HTML viewer, needing Tk *)
  Ctext.init();
  Attrs.init !Textw_fo.html_bg; (* built the bullet images *)
  (*e: [[Main.main()]] misc initialisation *)
  (*s: [[Main.main()]] html entities initialisation *)
  (* Initialization of HTML entities *)
  Html.init (Lang.lang());
  (*e: [[Main.main()]] html entities initialisation *)
  (*s: [[Main.main()]] applet system initialisation *)
  (* The applet system.
   * This loads the local modules also, so any setup that might be
   * overriden by a local module should happen before here.
   * However, preference initialisation must happen *after* initialisation
   * of the applet system
   *)
  !Version.applet_init !modules;
  (*e: [[Main.main()]] applet system initialisation *)
  (*s: [[Main.main()]] mmm server initialisation *)
  (* This must occur after most initialisations *)
  if !accept_external 
  then Cci.init();
  (*e: [[Main.main()]] mmm server initialisation *)
  (*e: [[Main.main()]] initialisation *)

  let url_opt = 
    match !init_urls with 
    | []     -> None 
    | x::_l -> Some x
  in
  let user_preferences_file =
    (*s: [[Main.main()]] user preferences file *)
    localize !preffile
    (*e: [[Main.main()]] user preferences file *)
  in
  (* Start the initial navigator *)
  Mmm.initial_navigator user_preferences_file url_opt;

  safe_loop();
  (*s: [[Main.main()]] after event loop, if debug mode *)
  if !Log.debug_mode then begin
    Cache.postmortem();
    Gcache.postmortem()
  end;
  (*e: [[Main.main()]] after event loop, if debug mode *)
  ()
(*e: function Main.main *)
      
(*s: function Main.postmortem *)
let postmortem () =
  try 
    main ()
  with
  | Dynlink.Error err ->
      failwith (spf "dynlink error = %s" (Dynlink.error_message err))
  | e -> 
      (*s: [[Main.main()]] after event loop, if debug mode *)
      if !Log.debug_mode then begin
        Cache.postmortem();
        Gcache.postmortem()
      end;
      (*e: [[Main.main()]] after event loop, if debug mode *)
      raise e
(*e: function Main.postmortem *)

(*s: toplevel Main._1 *)
let _ = 
  Printexc.catch postmortem ()
(*e: toplevel Main._1 *)
(*e: ./main.ml *)

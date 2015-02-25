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

(*s: function Main.main *)
let main () =

  Error.default := new Tk_error.t Widget.default_toplevel;
  Condition.backend := Tk_condition.backend ();
  Timer_.add_ref := (fun a b -> Timer.add a b |> ignore);
  Low.update_idletasks_backend := Tk.update_idletasks;
  Fileevent_.add_fileinput_ref := Fileevent.add_fileinput;
  Fileevent_.remove_fileinput_ref := Fileevent.remove_fileinput;
  Fileevent_.add_fileoutput_ref := Fileevent.add_fileoutput;
  Fileevent_.remove_fileoutput_ref := Fileevent.remove_fileoutput;
  Document.add_log_backend := Tk_document.add_log;
  Maps.broadcast_backend := Frx_synth.broadcast;

 (* As always, we must parse argument first, using references... *)
  let sufxfile = ref (Mmm.user_file "mime.types")
  and display = ref (try Sys.getenv("DISPLAY") with Not_found -> "")
  and preffile = ref (Mmm.user_file "MMM.ad")
  and init_urls = ref [] 
  and accept_external = ref false
  and palette = ref None
  and modules = ref true
  and clicktofocus = ref false
  in
  Arg.parse [
  "-proxy", Arg.String (fun s -> Http.proxy := s), 
  "<hostname>\tProxy host";
  "-port", Arg.Int (fun i -> Http.proxy_port := i),
  "<port>\t\tProxy port";
  "-d", Arg.String (fun s -> display := s),
  "<foo:0>\t\tDisplay";
  "-display", Arg.String (fun s -> display := s),
  "<foo:0>\tDisplay";
  "-suffixes", Arg.String (fun s -> sufxfile := s),
  "<file>\tSuffix file";
  "-external", Arg.Unit (fun () -> accept_external := true),
  "\t\tAccept remote command (mmm_remote <url>)";
  "-lang", Arg.String (fun s -> I18n.language := s),
  "<lang>\t\tI18n language";
  "-msgfile", Arg.String (fun s -> I18n.message_file := s),
  "<file>\tI18n message file";
  "-prefs", Arg.String (fun s -> preffile := s),
  "<file>\t\tPreference File";
  "-helpurl", Arg.String (fun s -> Mmm.helpurl := Lexurl.make s),
  "<url>\tHelp URL";
  "-palette", Arg.String (fun s -> palette := Some s),
  "<color>\tTk Palette";
  "-nomodule", Arg.Unit (fun () -> modules := false),
  "\t\tDon't load initial modules";
  "-clicktofocus", Arg.Unit (fun () -> clicktofocus := true),
  "\tClick to Focus mode (default is Focus Follows Mouse)";
  "-geometry", Arg.String (fun s -> Mmm.initial_geom := Some s),
  "<wxh+x+y>\tInitial geometry for the first navigator"
  ]
    (fun s -> init_urls := s :: !init_urls)
    "Usage: meuh <opts> <initial url>";

  Sys.catch_break true;
  (* Avoid SIGPIPE completely, in favor of write() errors *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

  let top = Tk.openTkDisplayClass !display "mmm" in

  (* Just after the init. of Tk, we have to detect the Tk is under
   * Latin or Japanese mode at first. 
   *)
  Lang.japan := Jtk.is_japanese_mode () && Lang.is_japanese ();
  (* Run Tcl in JIS (ISO2022-jp) Mode *)
  if !Lang.japan 
  then Jtk.Kanji.internal_code_set Jtk.JIS;

  Wm.withdraw top;
  
  if not !clicktofocus 
  then Focus.follows_mouse();

  (* Default values for navigator window *)
  Resource.add "*MMM.Width" "640" Tk.WidgetDefault;
  Resource.add "*MMM.Height" "480" Tk.WidgetDefault;

  (* Resources *)
  let site_resfile =
    localize (Filename.concat (Filename.dirname Sys.argv.(0)) "MMM.ad") in
  (* Site specific resource file usually in $INSTALLDIR=/usr/local/lib/mmm *)
  if Sys.file_exists site_resfile 
  then Tkresource.readfile site_resfile Tk.StartupFile;

  begin match !palette with
  | None -> ()
  | Some bg -> try Palette.set_background (Tk.NamedColor bg) with _ -> ()
  end;

  (* Initialisations in frx library : kbd navigation, search 
   * No prerequisite except Tk *)
  Frx_text.init ();
  (* Initialisations in jpf's balloon library *)
  Balloon.init ();
  (* Initialisations in jpf's GIF ANIMATION library *)
  Tkaniminit.f ();

  (* Local initialisations *)
  Low.init();                         (* start regular tasks *)
  Cache.init();                       (* builtin document *)
  Auth.init();                        (* start expiration timer *)
  Debug.init();                       (* debugging RPC *)
  
  (* Suffix mapping to Content-Type and Content-Encoding *)
  if Sys.file_exists !sufxfile 
  then Http_headers.read_suffix_file !sufxfile;
  
  (* Various stuff for the HTML viewer, needing Tk *)
  Ctext.init();
  Attrs.init !Textw_fo.html_bg; (* built the bullet images *)

  (* Initialization of HTML entities *)
  Html.init !Lang.japan;

  (* The applet system.
   * This loads the local modules also, so any setup that might be
   * overriden by a local module should happen before here.
   * However, preference initialisation must happen *after* initialisation
   * of the applet system
   *)
  !Version.applet_init !modules;

  (* This must occur after most initialisations *)
  if !accept_external 
  then Cci.init();

  (* Start the initial navigator *)
  ignore
    (Mmm.initial_navigator 
        (localize !preffile)
        (match !init_urls with | [] -> None | x :: l -> Some x));
  safe_loop();
  if !Log.debug_mode then begin
    Cache.postmortem();
    Gcache.postmortem()
  end
(*e: function Main.main *)
      
(*s: function Main.postmortem *)
let postmortem () =
  try 
    main ()
  with
  | Dynlink.Error err ->
      failwith (spf "dynlink error = %s" (Dynlink.error_message err))
  | e -> 
      if !Log.debug_mode then begin
        Cache.postmortem();
        Gcache.postmortem();
      end;
      raise e
(*e: function Main.postmortem *)

(*s: toplevel Main._1 *)
let _ = 
  Printexc.catch postmortem ()
(*e: toplevel Main._1 *)
(*e: ./main.ml *)

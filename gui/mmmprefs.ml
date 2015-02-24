(*s: ./gui/mmmprefs.ml *)
open Tk
open Prefs

(* MMM Preferences *)

(*s: function Mmmprefs.font_pref *)
(*
 * Font preference
 *)
let font_pref title name top = 
  let f = Frame.create top [] in
  let l = Label.create_named f "fontname" [Text title] in
  let f', v, i, s =
    Fontprefs.font_select f
      (fun () -> Styles.get_font name)  (* get from internal value *)
      (Styles.set_font name)		(* set internal value *)
  in
   pack [l;f'][Side Side_Left];
  (* map exceptions to error *)
  let i v = try i v with Failure s -> pref_error s
  and s v = try s v with Failure s -> pref_error s
  in
  let p = {
    pref_type = AbstractType(i,s);
    pref_variable = v;
    packed_widget = f;
    pref_name = title;
    resource_name = resource_name title} in
  p
(*e: function Mmmprefs.font_pref *)


(*s: constant Mmmprefs.image_loading *)
(*
 * Image loading mode
 *)
let image_loading =
  option_handlers 
    [ Imgload.AfterDocManual, "After document, manual"; 
      Imgload.AfterDocAuto, "After document, automatic";
      Imgload.DuringDoc, "During document loading"]
    (fun () -> !Imgload.mode)
    (fun v -> Imgload.mode := v)
(*e: constant Mmmprefs.image_loading *)


(*
 * Choose from available DTDs for HTML parsing 
 *)
let dtd_i v =
  Textvariable.set v (Dtd.name !Dtd.current)
and dtd_s v =
  Dtd.current := 
    try
      Dtd.get (Textvariable.get v)
    with
      Not_found -> Dtd.dtd32
and dtd_p = Dtd.names()


(*s: function Mmmprefs.network *)
let network top = 
  family top (I18n.sprintf "Protocols") [
    string_pref "Proxy host" Http.proxy;
    int_pref "Proxy port" Http.proxy_port;
    bool_pref "Always Use Proxy" Http.always_proxy;
    bool_pref "HTTP Send Referer" Http.send_referer;
    string_pref "User Agent" Http.user_agent;
    int_pref "Timeout on headers (seconds)" Http.timeout;
    int_pref "Password lifetime (minutes)" Auth.lifetime;
    string_pref "Password save file" Auth.auth_file;
    abstract_string_pref "Local binaries path" File.pref_init File.pref_set
    ]
(*e: function Mmmprefs.network *)

(*s: function Mmmprefs.internal *)
let internal top =
  family top (I18n.sprintf "Internal settings and debugging") [
    bool_pref "Strict encoding of Form field names" Urlenc.strict_form_standard;
    bool_pref "HTTP Requests" Http.verbose;
    int_pref "Internal buffer" Textw_fo.internal_buffer;
    bool_pref "General trace" Log.debug_mode;
    bool_pref "Scheduler" Scheduler.debug;
    bool_pref "Cache debug" Cache.debug;
    bool_pref "Widget Cache debug" Gcache.debug;
    bool_pref  "HTML Display log" Html_disp.verbose;
    bool_pref "Table debug" Table.debug;
    bool_pref "Text fit debug" Fit.debug;
    bool_pref "Image loading debug" Img.ImageData.verbose;
    bool_pref "CamlTk Debug" Protocol.debug;
    bool_pref "Japanese Parsing Debug" Japan.debug
    ]
(*e: function Mmmprefs.internal *)

(*s: function Mmmprefs.html *)
let html top = 
  family top (I18n.sprintf "HTML parsing and display") [
    option_pref "DTD" (dtd_i, dtd_s, dtd_p);
    bool_pref "Strict HTML lexing" Lexhtml.strict;
    bool_pref "Attempt tables" Html_disp.attempt_tables;
    bool_pref "Ignore relative TD width" Table.strict_32;
    bool_pref "Attempt smooth scroll" Htmlw.pscrolling;
    bool_pref "Frames as links" Htmlw.frames_as_links;
    abstract_string_pref "Background color"
      (fun v -> Textvariable.set v !Textw_fo.html_bg)
      (fun v ->
     let color = Textvariable.get v in
        Textw_fo.html_bg := color;
      (* transparent GIF hack, for the initial images *)
        Textvariable.set (Textvariable.coerce "TRANSPARENT_GIF_COLOR")
                         color;
            (* set the resource for each possible class of embedded windows *)
        Resource.add "*Html*Text.background" color WidgetDefault;
            Resource.add "*Html*Message.background" color WidgetDefault;
            Resource.add "*Html*Label.background" color WidgetDefault;
            Resource.add "*Html*Listbox.background" color WidgetDefault;
            Resource.add "*Html*Button.background" color WidgetDefault;
            Resource.add "*Html*Entry.background" color WidgetDefault;
            Resource.add "*Html*Menubutton.background" color WidgetDefault;
            Resource.add "*Plain*Text.background" color WidgetDefault
        );
    string_pref "Entry and Textarea color" Form.form_bg;
    bool_pref "Follow document colors" Textw_fo.usecolors; 
    font_pref "Default font" "default";
    font_pref "<H1> font" "header1";
    font_pref "<H2> font" "header2";
    font_pref "<H3> font" "header3";
    font_pref "<H4> font" "header4";
    font_pref "<H5> font" "header5";
    font_pref "<H6> font" "header6";
    font_pref "Bold"   "bold";
    font_pref "Italic" "italic";
    font_pref "Fixed" "verbatim"
    ]
(*e: function Mmmprefs.html *)

(*s: function Mmmprefs.i18n *)
let i18n top =
  family top (I18n.sprintf "Internationalization (Japanese)") [
    (* bool_pref "Japanese mode" Version.japan; *)
    bool_pref "Ignore META charset" Htmlw.ignore_meta_charset
  ] 
(*e: function Mmmprefs.i18n *)

(*s: function Mmmprefs.images *)
let images top =
  family top (I18n.sprintf "Images") [
    bool_pref "No images at all" Imgload.no_images;
    option_pref "Image loading" image_loading;
       (* image_loading_i image_loading_s image_loading_p; *)
    int_pref "Max image connections" Img.ImageScheduler.maxactive;
    int_pref "Max image connections (same host)" Img.ImageScheduler.maxsamehost;
    float_pref "Gamma correction" Img.ImageData.gamma;
    string_pref "JPEG converter"  Img.ImageData.jpeg_converter
    ]
(*e: function Mmmprefs.images *)
    

(*s: function Mmmprefs.cache *)
let cache top =
  family top (I18n.sprintf "Cache settings") [
    int_pref "Max number of documents"  Cache.max_documents;
    int_pref "Delete how much when full" Cache.cleann;
    bool_pref "Keep only history" Cache.history_mode;
    int_pref "Max cached widgets per window" Gcache.max_keep
    ]
(*e: function Mmmprefs.cache *)

(*s: function Mmmprefs.progs *)
let progs top =
  family top (I18n.sprintf "External programs") [
    string_pref "Mailto program" Mailto.mailer;
    string_pref "Hotlist program" Hotlist.program;
    string_pref "Printing program" Save.print_command;
    ]
(*e: function Mmmprefs.progs *)

(*s: function Mmmprefs.misc *)
let misc top =
  family top (I18n.sprintf "Misc. settings") [
    bool_pref "Use balloon helps" Balloon.flag;
    bool_pref "Use GIF animation" Img.gif_anim_load;
    bool_pref "Automatic GIF animation display" Imgload.gif_anim_auto
    ]
(*e: function Mmmprefs.misc *)

(*s: constant Mmmprefs.appsys_plug *)
(* The default appsys preference only keeps track of
   the preference values, but does not allow changes
 *)
let appsys_plug = ref (fun top -> 
  let f = Frame.create top [Relief Sunken; BorderWidth (Pixels 1)] in
  let t = Label.create f [Text (I18n.sprintf "Applets")] in
  let msg =
   Message.create f [Text (I18n.sprintf "Applets are not available \
                                         in the native version")] in
  pack [t][Side Side_Top];
  pack [msg][Side Side_Bottom];
  (* we must keep track of applet preferences in the 
     bytecode version : "Active" and "Paranoid" *)
  let active = ref false  and active_name = resource_name "Active"
  and paranoid = ref true and paranoid_name = resource_name "Paranoid" in
  let init () = ()  (* nothing special to be done *)
  and save () =
    List.fold_right 
      (fun (name,value) map -> PrefMap.add name value map)
      [active_name, (if !active then "1" else "0");
       paranoid_name, (if !paranoid then "1" else "0")]
      PrefMap.empty
  and load () = 
    List.iter (fun (name, setf) ->
      try
    let prefdata = Resource.get Widget.default_toplevel name name in
    setf prefdata
      with
    Not_found -> ())
      [active_name, (function data -> active := data = "1");
       paranoid_name, (function data -> paranoid := data = "1")]
  in
  {family_widget = f; family_init = init;
   family_save = save; family_load = load;
   family_title = I18n.sprintf "Applets"})
(*e: constant Mmmprefs.appsys_plug *)

(*s: function Mmmprefs.plug_applets *)
let plug_applets f =
  appsys_plug := f
(*e: function Mmmprefs.plug_applets *)

(*s: function Mmmprefs.applets *)
let applets w = !appsys_plug w
(*e: function Mmmprefs.applets *)


(*s: constant Mmmprefs.home *)
(* There is no right place for this *)
let home = ref ""
(*e: constant Mmmprefs.home *)
(*s: function Mmmprefs.reset_home *)
let reset_home () =
  home :=  Tkresource.string "wwwHome" 
       (try Sys.getenv "WWW_HOME"
       with Not_found -> (Version.initurl (Lang.lang ())))
(*e: function Mmmprefs.reset_home *)


(*s: constant Mmmprefs.mute *)
(* Internal preferences *)
let mute = [
  reset_home;
  Fonts.reset;
  Viewers.reset;			(* viewers definition *)
  Glevents.reset;			(* bindings *)
  ]
(*e: constant Mmmprefs.mute *)

(*s: constant Mmmprefs.families *)
(* Interactive preferences *)
let families = [ network; html; i18n; images; progs; cache; applets;
         misc; internal ]
(*e: constant Mmmprefs.families *)

(*s: function Mmmprefs.f *)
let f preffile = Prefs.define preffile families mute
(*e: function Mmmprefs.f *)
(*e: ./gui/mmmprefs.ml *)

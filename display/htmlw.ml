(*s: ./display/htmlw.ml *)
open I18n
open Printf
open Tk
open Html
open Html_eval
open Hyper
open Document
open Viewers
open Htmlfmt
open Feed
open Embed
open Htframe

(*s: constant Htmlw.frames_as_links *)
(* Prefs globals *)
let frames_as_links = ref false
(*e: constant Htmlw.frames_as_links *)
(*s: constant Htmlw.pscrolling *)
let pscrolling = ref false
(*e: constant Htmlw.pscrolling *)
(*s: constant Htmlw.ignore_meta_charset *)
let ignore_meta_charset = ref false
(*e: constant Htmlw.ignore_meta_charset *)

(*s: constant Htmlw.scroll_icon *)
let scroll_icon ="
#define mini-scroll-arrows_width 16
#define mini-scroll-arrows_height 14
static char mini-scroll-arrows_bits[] = {
 0x80,0x00,0xc0,0x01,0xc0,0x01,0xe0,0x03,0xf0,0x07,0xf0,0x07,0x00,0x00,0x00,
 0x00,0xf0,0x07,0xf0,0x07,0xe0,0x03,0xc0,0x01,0xc0,0x01,0x80,0x00};
"
(*e: constant Htmlw.scroll_icon *)

(*s: constant Htmlw.scroll_image *)
let scroll_image = 
  lazy (ImageBitmap(Imagebitmap.create [Data scroll_icon]))
(*e: constant Htmlw.scroll_image *)

(*s: module Htmlw.F *)
module F = Html_disp.Make(Textw_fo)(Form)(Table)
(*e: module Htmlw.F *)


(*s: function Htmlw.progress_report *)
(* Builds the progress report and pointsto zone.
 * Adds ctx nav function for pointsto
 *)
let progress_report top ctx =
  let f = Frame.create_named top "progress" [] in
  let pointstov = Textvariable.create_temporary f in
  let pointsto = Textvariable.set pointstov in
  let lpoint = 
    Label.create_named f "pointsto" [TextVariable pointstov; Anchor W]
  and fprog = Frame.create_named f "fr" [Width (Pixels 200); Height (Pixels 5)]
  in
  (* progress meter requires an alt widget, but we don't have to pack it *)
  let fakealt = Label.create_named fprog "alt" [] in
  pack [fprog][Side Side_Left];
  pack [lpoint][Side Side_Left; Fill Fill_X];
  (* hack to avoid lpoint forcing the navigator to grow like hell *)
  Frame.configure f
    [ Width (Pixels (Winfo.reqwidth (Winfo.toplevel f)));
      Height (Pixels (Winfo.reqheight lpoint))];
  Pack.propagate_set f false;

  ctx#add_nav ("pointsto" ,
           { hyper_visible = false;
         hyper_title = "Show target";
         hyper_func = (fun _ h -> 
           let target = 
             try Hyper.string_of h
             with Invalid_link msg -> "invalid link" in
           pointsto target)});
  ctx#add_nav ("clearpointsto" ,
           { hyper_visible = false;
         hyper_title = "Clear target";
         hyper_func = (fun _ h -> pointsto "")
           });
  
  f, Tk_progress.meter fprog
(*e: function Htmlw.progress_report *)

(*s: function Htmlw.html_head_ui *)
let html_head_ui headers redisplay pscroll top ctx =
  (* The frame for all head UI elements *)
  let headgroup = Frame.create_named top "head" [] in
  (* The menubar frame *)
  let bargroup = Frame.create_named headgroup "menubar" [] in
  let titlev = Textvariable.create_temporary headgroup in

  let headersb = Menubutton.create_named bargroup "headers" 
      [TextVariable titlev; TextWidth 80]
  in
  let headersm = Menu.create_named headersb "menu" [] in
  Menubutton.configure headersb [Menu headersm];

  (* The link button and menu *)
  let linkb = 
    Menubutton.create_named bargroup "links" [Text "Links"; State Disabled] in
  let linkmenu = Menu.create_named linkb "linkmenu" [] in
  Menubutton.configure linkb [Menu linkmenu];

  (* The scroll-mode button *)
  let scrollv = Textvariable.create_temporary bargroup in
  Textvariable.set scrollv 
      (if !pscroll then "1" else "0");
  let scrollb = 
    Checkbutton.create_named bargroup "smoothScroll" 
      [ Variable scrollv; Lazy.force scroll_image;
    IndicatorOn false; Command (fun () ->
      match Textvariable.get scrollv with
        "1" -> pscroll := true; redisplay()
      | _ -> pscroll := false; redisplay())]
  in
  (* bargroup IN THIS ORDER -- RESIZING *)
  pack [linkb][Side Side_Right];
  pack [scrollb][Side Side_Right; Fill Fill_Y];
  pack [headersb][Side Side_Left; Fill Fill_X; Expand true];
  pack [bargroup][Side Side_Top; Fill Fill_X];

  let set_title t =
    let tl = Winfo.toplevel top
    and title = s_ "MMM Browser@%s" t in
    if Widget.known_class tl = "toplevel" then
      (Wm.title_set tl title; Wm.iconname_set tl title);
    Textvariable.set titlev t

  and add_link title hlink = 
    Menubutton.configure linkb [State Normal];
    Menu.add_command linkmenu [ Label title; 
                Command (fun () -> ctx#goto hlink)]
      
  in
  
  (* Extra headers: META tags should be parsed by *servers*, not by clients.
     TODO: find interface so we can export this feature to applets/modules
     *)
  let sep_added = ref false in
  let add_header h v =
    if not !sep_added then (sep_added := true; Menu.add_separator headersm);
    let txt = sprintf "%s: %s" h v in
    match String.lowercase h with
      "refresh" ->
       begin try
          let pos = String.index v ';'
          and pos2 = String.index v '=' in
          let delay = int_of_string (String.sub v 0 pos)
          and url = String.sub v (pos2+1) (String.length v - pos2 - 1) in
      Menu.add_command headersm 
        [Label txt;
          Command (fun () -> ctx#goto {
        h_uri = url;
        h_context = Some (Url.string_of ctx#base.document_url);
        h_method = GET;
            h_params = []})]
    with Not_found | Failure "int_of_string" -> ()
    end
    | _ -> Menu.add_command headersm [Label txt] 

  in
  (* the head menu is a good place to put some other information and ui *)
  (* you must be sure that this function is used after all the head ui
   * stuff using add_header are finished. 
   *)  
  let sep_extra_added = ref false in 
  let add_extra_header f =
    if not !sep_extra_added then
       (sep_extra_added := true; Menu.add_separator headersm);
    f headersm
  in

  set_title (Url.string_of ctx#base.document_url);
  List.iter (function h -> Menu.add_command headersm [Label h])
    (List.rev headers);
  
  headgroup, set_title, add_link, add_header, add_extra_header
(*e: function Htmlw.html_head_ui *)


(*s: function Htmlw.ignore_open *)
(*
 * Extend a display machine to interpret HEAD elements with
 * an influence on the HEAD ui display.
 * NOTE: some other HEAD elements interpretation *must* be done
 * even if we don't have UI for HEAD (e.g. base)
 *)
let ignore_open _ _ = ()
(*e: function Htmlw.ignore_open *)
(*s: function Htmlw.ignore_close *)
let ignore_close _ = ()
(*e: function Htmlw.ignore_close *)

(*s: function Htmlw.head_hook *)
let head_hook (headgroup,set_title,add_link,add_header) mach =
  mach#add_tag "title" 
    (fun fo t ->
      mach#push_action
    (fun s -> 
      set_title (Html.beautify2 s);
      mach#pop_action))
    ignore_close;
      
  mach#add_tag "isindex"
    (fun fo tag ->
      let prompt = get_attribute tag "prompt" in
      let action s =
    mach#ctx#goto { h_uri = "?" ^ Urlenc.encode s;
               h_context = Some mach#base;
               h_method = GET;
               h_params = []} in
      let f,e = Frx_entry.new_label_entry headgroup prompt action in
      pack [f] [Fill Fill_X])
    ignore_close;

  mach#add_tag "link"
    (fun fo tag ->
      try
        let href = get_attribute tag "href" in
    let name = 
      try get_attribute tag "title"
      with Not_found ->
        try get_attribute tag "rel"
        with Not_found -> 
          try get_attribute tag "rev"
          with Not_found -> href in
    let h_params =
      try ["target", get_attribute tag "target"]
      with
        Not_found ->
          match mach#target with
        Some s -> ["target", s]
          |	None -> []
        in
        add_link name { h_uri = href; h_context = Some mach#base;
            h_method = GET; h_params = h_params}
      with
    Not_found -> () (* no href *))
    ignore_close;

  begin
    let old =
      try
    let oldo, c = mach#get_tag "meta" in oldo
      with
    Not_found -> ignore_open in
    mach#add_tag "meta"
      (fun fo tag ->
       try 
      old fo tag;
      add_header 
        (get_attribute tag "http-equiv")
        (get_attribute tag "content")
       with Not_found -> ())
      ignore_close;
  end;

  (* Non standard extensions *)
  if !frames_as_links  then
    mach#add_tag "frame"
      (fun fo tag ->
    try
         let src = get_attribute tag "src" in
         let name =
        sprintf "Frame %s" 
          (try get_attribute tag "name"
          with Not_found -> "unnamed")
      in
      add_link name { h_uri = src; h_context = Some mach#base;
              h_method = GET; h_params = []}
    with
      Not_found -> () (* no src *))
      ignore_close
(*e: function Htmlw.head_hook *)

(* This class only defines globals *)
class  virtual viewer_globs ((ctx : Viewers.context),
                (dh' : Document.handle)) =
 object

  (* copy params *)
  (* val ctx = ctx *)           
  method ctx = ctx

  val mutable dh = dh'
  method dh = dh

  val did = dh'.document_id
  method did = did
end

(* We still need dh at construction for the definition of feed_red *)
class  virtual html_parse (dh) =
 object (self)
  (* red tape for progress report *)
  val mutable red = 0
  val mutable size = try Some (Http_headers.contentlength dh.document_headers)
    with Not_found -> None (* duh *);
  val mutable feed_read = new Japan.read_i18n (fun s o n -> 0)

  val mutable (*private*) lexbuf = Lexing.from_string "" (* duh *)
  method lexbuf = lexbuf  

  val mutable lexer = sgml_lexer !Dtd.current

  (* Japanese parse configuration *)
  val jpn_config = Japan.default_config ()

  method parse_init =
    red <- 0;
    feed_read <-
       Japan.create_read_native self#dh.document_feed.feed_read;
    (* Q: do we need to restart a new sgml_lexer ? *)
    lexer <- sgml_lexer !Dtd.current;
    lexbuf <- 
       Lexing.from_function (fun buf n -> 
     let r = feed_read#read buf 0 n in
     red <- red + r;
     self#set_progress size red;
     r)
end

class  virtual html_body () =
 object (self)

  method virtual mach : Html_disp.machine

  val current_scroll_mode = ref !pscrolling
  method current_scroll_mode = current_scroll_mode

  (* hack for frames on incorrect html *)
  val mutable body_frame = None
  method body_init full =
    (* Install a tag handler for body that will actually create the
       a formatter and install it *)
    let body_formatter = ref None in
    self#mach#add_tag "body" 
      (fun fo t ->
    match !body_formatter with
    | None -> (* it's the first body *)
        let format, fhtml = 
          self#mach#create_formatter 
           (if full then (TopFormatter !current_scroll_mode) 
           else FrameFormatter self#ctx#params)
           self#frame
        in
        self#mach#push_formatter format;
        body_formatter := Some format;
        body_frame <- Some fhtml;
        pack [fhtml][Side Side_Left; Expand true; Fill Fill_Both];
         List.iter (function
          | "bgcolor", color -> 
             format.set_defaults "background" [BgColor color]
          | "text", color -> 
          format.set_defaults "foreground" [FgColor color]
          | "link", color ->
          format.set_defaults "link" [FgColor color]
          | "alink", color ->
          format.set_defaults "alink" [FgColor color]
          | "vlink", color ->
          format.set_defaults "vlink" [FgColor color]
          | _,_ -> ())
             t.attributes
    | Some f -> (* multiple body... *)
        self#mach#push_formatter f
     )
      (fun t -> 
    ignore self#mach#pop_formatter)
end

(* geek stuff *)
class  virtual bored () =
 object (self)
  method bored_init hgbas =
    let bored = 
      Resource.get Widget.default_toplevel "bored" "bored" = "yes"
    || begin try
       ignore (
         Str.search_forward (Str.regexp_case_fold "sandra") self#mach#base 0);
       Resource.add "*bored" "yes" Interactive;
       true
    with Not_found -> false
    end in
    if bored then begin
      let b = Button.create hgbas [
       Text "\182"; BorderWidth (Pixels 0);
       Command (fun () ->
      self#ctx#goto 
       (Hyper.default_link  
         "http://www.columbiatristar.co.uk/the_net/contents.html")
      )] in
      let l  = Winfo.children hgbas in
      pack [b][After (List.hd l); Side Side_Right]
    end

end

(*s: class Htmlw.display_html *)
class display_html ((top : Widget.widget),
                    (ctx : Viewers.context),
                    (mediapars : (string * string) list),
                    (imgmanager: Imgload.loader),
                    (dh': Document.handle)) =
 object (self)
  inherit Viewers.display_info () as di  (* gives us basic features *)
  inherit viewer_globs (ctx, dh')
  inherit html_parse (dh')
  inherit html_body () as body
  inherit bored ()

  (* val imgmanager = imgmanager *)

  (*s: [[Htmlw.display_html]] private fields *)
  val mutable init_mode = true
  val mutable pending = true
  (*x: [[Htmlw.display_html]] private fields *)
  val mutable mach = F.create (ctx, imgmanager)
  (*x: [[Htmlw.display_html]] private fields *)
  val (*private*) annotations = ref []
  (*x: [[Htmlw.display_html]] private fields *)
  val mutable add_extra_header = fun f -> ()
  (*x: [[Htmlw.display_html]] private fields *)
  val (*private*) errors = ref []
  (*x: [[Htmlw.display_html]] private fields *)
  val mutable (*private*) terminated = false
  (*x: [[Htmlw.display_html]] private fields *)
  val mutable set_progress = Progress.no_meter
  (*e: [[Htmlw.display_html]] private fields *)

  (*less: can be changed? *)
  val mutable title = Url.string_of ctx#base.document_url
  method di_title = title

  (*s: [[Htmlw.display_html]] frame widget methods *)
  val frame = 
    if not (Winfo.exists top) 
    then failwith "too late"
    else Frame.create_named top (Mstring.gensym "html") [Class "Html"]
      (* this might as well fail if the window was destroyed before we
       * finally could get the headers of the document.
       *)
  method frame = frame
  method di_widget = frame
  (*e: [[Htmlw.display_html]] frame widget methods *)
  (*s: [[Htmlw.display_html]] init method *)
  (* since we may be called multiple times, we have to clear some of the
     instance variables *)
  method init full =
    (*s: [[Htmlw.display_html]] reset private fields *)
    init_mode <- full;
    pending <- true;

    errors := []; 
    annotations := []; 
    terminated <- false; 
    set_progress <- Progress.no_meter;

    mach <- F.create (ctx, imgmanager);
    (*e: [[Htmlw.display_html]] reset private fields *)

    (*s: [[Htmlw.display_html]] set meta tag *)
    let meta_charset = ref None in
    (* <META HTTP-EQUIV="Content-Type" CONTENT="*/*;CHARSET=*"> stuff *)
    if not !ignore_meta_charset then begin 
      mach#add_tag "meta" (fun fo tag ->
       try 
         let h = get_attribute tag "http-equiv" in
         let v = get_attribute tag "content" in
         match String.lowercase h with
         | "content-type" ->
              begin try
                let (t,h), l = Lexheaders.media_type v in
                if String.lowercase t <> "text" ||
                   String.lowercase h <> "html" then begin
                     Log.f ("Unknown meta content-type = "^t^"/"^h);
                    raise Exit
                end;
                try 
                  List.iter (fun (h,v) ->
                    if String.lowercase h = "charset" then begin
                      let v = String.lowercase v in
                      Log.f ("MetaCharset detect : " ^ v);
                      begin try
                        let code = 
                          let code = ref Japan.Unknown in
                          try List.iter (fun (x,c) -> 
                             if Str.string_match (Str.regexp x) v 0 then begin
                               code := c;
                               raise Exit
                             end
                             ) Japan.encode_table ;
                             raise Not_found
                          with Exit -> !code
                        in
                        (* feed_read#set_code code; this does not work... *)
                        meta_charset := Some code
                      with Not_found ->
                        Log.f (v ^ ": I don't know this charset")
                      end;
                      raise Exit
                   end) l;
                with Exit -> ()
              with _ -> () (* if failed to parse, ignore it *)
            end
         | _ -> ()
       with Not_found -> ()
     )
     ignore_close
      end;
    (*e: [[Htmlw.display_html]] set meta tag *)

    (*s: [[Htmlw.display_html]] set progress report and head UI *)
    (* We have full display, so put up progress report and head UI *)
    if full then begin 
      let hgbas, progf = progress_report frame ctx in
      set_progress <- progf;
      self#bored_init hgbas;
      pack [hgbas] [Side Side_Bottom; Fill Fill_X];
      let headgroup, set_title, add_link, add_header, add_ext_header =
        html_head_ui dh.document_headers (fun () -> self#redisplay)
        self#current_scroll_mode frame ctx
      in
      add_extra_header <- add_ext_header;
      pack [headgroup] [Side Side_Top; Fill Fill_X];
      let set_title s = 
        title <- s; 
        set_title s 
      in
      head_hook (headgroup, set_title, add_link, add_header) self#mach
    end;
    (*e: [[Htmlw.display_html]] set progress report and head UI *)

    self#body_init full;

    (*s: [[Htmlw.display_html]] display frames *)
    if not !frames_as_links then
      Htframe.add_frames (self#load_frames) (fun () ->
        match body_frame with
        | None -> ()
        | Some f -> Tk.destroy f
      ) frame mach;
    (*e: [[Htmlw.display_html]] display frames *)

    (* Asynchronous parsing and display, token by token *)
    self#parse_init;

    (*s: [[Htmlw.display_html]] i18 encoder for forms *)
    (* I18n encoder for Forms *)
    (*
    if !Lang.japan then begin
      mach#set_i18n_encoder (fun s -> Japan.encoder feed_read#get_code s)
    end;
    *)
    (*e: [[Htmlw.display_html]] i18 encoder for forms *)

    dh.document_feed.feed_schedule (fun () ->
      try 
        let warnings, correct,   tokens, loc = 
          lexer self#lexbuf 
        in
        (*s: [[Htmlw.display_html]] in feed, record possible errors after lexing *)
        warnings |> List.iter (fun (reason, pos) -> 
          self#record_error (Loc(pos,succ pos)) reason
        );
        (match correct with
        | Legal -> ()
        | Illegal reason -> self#record_error loc reason
        );
        (*e: [[Htmlw.display_html]] in feed, record possible errors after lexing *)

        (*s: [[Htmlw.display_html]] in feed, annotate *)
        (* We annotate only the last token, which is normally the one
         * from the original token stream *)
        let rec annot_last = function
        | [] -> ()
        | [x] -> self#annotate loc x
        | x::l -> annot_last l
        in
        annot_last tokens;
        (*e: [[Htmlw.display_html]] in feed, annotate *)
         tokens |> List.iter (fun token -> 
           begin  
             try mach#send token
             with Invalid_Html s -> self#record_error loc s
           end;
           if token = EOF then begin
             pending <- false;
             imgmanager#flush_images;
             raise End_of_file
           end
         )
      with 
      | End_of_file ->
          (*s: [[Htmlw.display_html]] in feed, End_of_file exn handler *)
          (*s: [[Htmlw.display_html]] in feed, End_of_file exn handler, if japan *)
          (*e: [[Htmlw.display_html]] in feed, End_of_file exn handler, if japan *)
          self#set_progress (Some red) red;
          self#finish false;
          mach#see_frag dh.document_fragment;
          (*s: [[Htmlw.display_html]] in feed, End_of_file exn handler, if japn and charset *)
          (*e: [[Htmlw.display_html]] in feed, End_of_file exn handler, if japn and charset *)
          (*e: [[Htmlw.display_html]] in feed, End_of_file exn handler *)
      (*s: [[Htmlw.display_html]] in feed, other exceptions handler *)
      | Html_Lexing (s,n) ->
          (* this should not happen if Lexhtml was debugged *)
          self#record_error (Html.Loc(n,n+1)) s
      (*x: [[Htmlw.display_html]] in feed, other exceptions handler *)
      | Unix.Unix_error(_,_,_) ->
          self#finish true
      | e ->
          Log.f (sprintf "FATAL ERROR (htmlw) %s" (Printexc.to_string e));
          self#finish true
      (*e: [[Htmlw.display_html]] in feed, other exceptions handler *)
     )
  (*e: [[Htmlw.display_html]] init method *)

  (*s: [[Htmlw.display_html]] fragment methods *)
  method di_fragment = 
    mach#see_frag
  (*e: [[Htmlw.display_html]] fragment methods *)
  (*s: [[Htmlw.display_html]] load images methods *)
  method mach = mach
  (*x: [[Htmlw.display_html]] load images methods *)
  method di_load_images = 
    (* load our images *)
    imgmanager#load_images;

    (* Recursively, for all embedded objects, send the load_images event.
     * Because we work on the children of the frame, the *currently* 
     *  displayed document in this frame gets the event.
     * NOTE: because of textvariable handlings, we can NOT send the
     *  event again during the processing of the event... 
     *)
    Frx_after.idle (fun () ->
      mach#embedded |> List.iter (fun { embed_frame = f} ->
        Winfo.children f |> List.iter (Frx_synth.send "load_images")
      )
    )
  (*e: [[Htmlw.display_html]] load images methods *)
  (*s: [[Htmlw.display_html]] update embedded objects methods *)
  method di_update =
    imgmanager#update_images;

    Frx_after.idle (fun () ->
      mach#embedded |> List.iter (fun {embed_frame = f} ->
        Winfo.children f |> List.iter (Frx_synth.send "update")
      )
    )
  (*e: [[Htmlw.display_html]] update embedded objects methods *)
  (*s: [[Htmlw.display_html]] load frames methods *)
  method load_frames frames =
    (* all targets defined in all framesets in this document *)
    let targets = 
      frames |> List.map (fun (frdesc, w) -> frdesc.frame_name, w) 
    in
    frames |> List.iter (fun (frdesc, w) ->
      let thistargets =
           ("_self", w) (* ourselves *)
        :: ("_parent", Winfo.parent w) (* our direct parent *)
        :: targets (* common targets *)
      in
      let ectx = ctx#for_embed frdesc.frame_params thistargets in
      (* add frame parameters and targets to our ctx *)
      (* NOTE: there is a redundancy between embed_frame and _self in ctx,
         but frames are only an instance of embedded objects so we should
         no rely on the existence of _self for embed display machinery *)
       mach#add_embedded {
         embed_hlink = { 
           h_uri = frdesc.frame_src;
           h_context = Some (Url.string_of ctx#base.document_url);
           h_method = GET;
           h_params = []
         };
         embed_frame = w;
         embed_context = ectx;
         embed_map = Maps.NoMap;
         embed_alt = frdesc.frame_name
       }
    )
  (*e: [[Htmlw.display_html]] load frames methods *)

  (*s: [[Htmlw.display_html]] error managment methods *)
  (* error reporting *)
  method record_error loc msg =
    errors := (loc,msg) :: !errors;
    self#set_progress size (-1)
  (*e: [[Htmlw.display_html]] error managment methods *)

  (*s: [[Htmlw.display_html]] progress method *)
  (* progress report *)
  method set_progress = set_progress
  (*e: [[Htmlw.display_html]] progress method *)
  (*s: [[Htmlw.display_html]] abort methods *)
  method di_abort = 
    self#finish true

  (* [finish abort?] *)
  method finish abort =
    if not terminated then begin
      terminated <- true;
      ctx#log (if abort then "Aborted" else "");
      Document.dclose true dh
    end;
    (* This has to happen even if we already finished displaying the document *)
    if abort then begin
      Img.ImageScheduler.stop dh.document_id;
      Embed.EmbeddedScheduler.stop dh.document_id
      (* TODO we should also require embedded objects to abort *)
    end
  (*e: [[Htmlw.display_html]] abort methods *)
  (*s: [[Htmlw.display_html]] redisplay methods *)
  method di_redisplay = 
    self#redisplay

  (* to redisplay, we have to destroy all widgets, then restart, except
     that we don't use the initial feed, but rather the cache *)
  method redisplay =
    if pending 
    then Error.f (s_ "Cannot redisplay document (pending)")
    else
      try
        dh <- Decoders.insert (Cache.renew_handle dh);
        Winfo.children frame |> List.iter destroy;
        self#init init_mode
      with Not_found ->
        Error.f (s_ "Document not in cache anymore")
  (*e: [[Htmlw.display_html]] redisplay methods *)
  (*s: [[Htmlw.display_html]] destroy methods *)
  method di_destroy = 
    if Winfo.exists frame 
    then Tk.destroy frame;
  (*e: [[Htmlw.display_html]] destroy methods *)
  (*s: [[Htmlw.display_html]] other methods or fields *)
  method annotate loc = function
    | OpenTag {tag_name=name} ->
        annotations := (name, loc) :: !annotations
    | CloseTag name ->
        annotations := (name, loc) :: !annotations
    | _ -> ()
  (*x: [[Htmlw.display_html]] other methods or fields *)
  method add_extra_header = add_extra_header
  (*x: [[Htmlw.display_html]] other methods or fields *)
  method di_source = self#source

  (* The source is attached to this frame so we can destroy the interior widgets*)
  method source =
    if pending 
    then Error.f (s_ "Cannot view document source (pending)")
    else Source.view frame did (fun () -> self#redisplay) errors annotations
           feed_read#get_code
  (*e: [[Htmlw.display_html]] other methods or fields *)

end
(*e: class Htmlw.display_html *)
      
(*s: function Htmlw.display_html *)
let display_html mediapars top ctx dh =
  let imgmanager = Imgload.create() in
  let viewer = new display_html (top,ctx,mediapars,imgmanager,dh) in
  viewer#init true;
  Some (viewer :> Viewers.display_info)
(*e: function Htmlw.display_html *)

(*s: function Htmlw.embedded_html *)
(* TODO: we should be able to share the imgmanager, but I don't see
 *  where we can get it from (except by adding something in ctx)
 *)
let embedded_html mediapars top ctx doc =
  let imgmanager = Imgload.create() in
  let dh = Decoders.insert (Cache.make_embed_handle doc) in
  let ctx = ctx#in_embed dh.document_id in
  let viewer = new display_html (top,ctx,mediapars,imgmanager,dh) in
  viewer#init false;
  pack [viewer#di_widget][Expand true; Fill Fill_Both];
  (* set for events *)
  Frx_synth.bind viewer#di_widget "load_images" 
    (fun top -> viewer#di_load_images);
  Frx_synth.bind viewer#di_widget "update" 
    (fun _ -> Embed.update top ctx doc (fun () -> viewer#di_update))
(*e: function Htmlw.embedded_html *)

(*s: toplevel Htmlw._1 *)
let _ =
  Viewers.add_builtin ("text","html") display_html
(*e: toplevel Htmlw._1 *)
(*s: toplevel Htmlw._2 *)
let _ =
  Embed.add_viewer ("text", "html") embedded_html
(*e: toplevel Htmlw._2 *)

(*e: ./display/htmlw.ml *)

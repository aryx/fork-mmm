(*s: ./viewers/plain.ml *)
open Tk
open Unix
open Frx_text
open Document
open Viewers
open Feed

(*s: class Plain.plain *)
class plain ((top : Widget.widget),
             (ctx : Viewers.context),
             (dh : Document.handle)) =
 object (self)
  inherit Viewers.display_info () as di  (* gives us basic features *)
  inherit Htmlw.viewer_globs (ctx,dh)

  val frame = 
     if not (Winfo.exists top) 
     then failwith "too late"
     else Frame.create top [Class "Plain"]
  method frame = frame
  method di_widget = frame
  
  (* to redisplay, we have to destroy all widgets, then restart, except
     that we don't use the feed, but rather the cache *)
  method redisplay =
    try
      dh <- Decoders.insert (Cache.renew_handle dh);
      Winfo.children frame |> List.iter destroy;
      self#init
    with Not_found ->
       Error.default#f (I18n.sprintf "Document not in cache anymore")

  (* [finish abort?] *)
  val mutable (*private*) terminated = false
  method finish abort =
    if not terminated then begin
      terminated <- true;
      self#ctx#log (if abort then "Aborted" else "");
      dclose true dh;
    end

  val mutable (*private*) tw = Widget.default_toplevel

  (* progress report *)
  val mutable set_progress = Progress.no_meter
  method set_progress = set_progress


  val mutable pending = true

  method add_text s =
    if s = "" 
    then pending <- false 
    else
      if Winfo.exists tw then begin
        Text.configure tw [State Normal];
        Text.insert tw textEnd s [];
        Text.configure tw [State Disabled]
      end

  method init =
    let hgbas, progf = Htmlw.progress_report frame ctx in
    set_progress <- progf;
    pack [hgbas] [Side Side_Bottom; Fill Fill_X];
    let (headgroup,_,_,_,_) = 
      Htmlw.html_head_ui dh.document_headers (fun () -> ()) (ref false)
       frame ctx 
    in 
    pack [headgroup][Side Side_Top; Fill Fill_X];
    (* Scrollable text widget *)
    let hgroup = Frame.create_named frame "textw" [Class "Plain"] in
    let ftext, text = 
      Frx_text.new_scrollable_text hgroup [Wrap WrapWord; State Disabled] true 
    in
    (* Tk4.0pl3 fix, + avoid cb to scrollbar *)
    Text.configure text [TakeFocus true; InsertOffTime 0];
    Frx_text.addsearch text;
    (* IN THIS ORDER -- RESIZING *)
    pack [ftext][Side Side_Left; Fill Fill_Both; Expand true];
    pack [hgroup][Fill Fill_Both; Expand true];

    (* pick up the fixed font *)
    let attrs_fixed = Styles.get_font "fixed" in
    let attrs_default = Styles.get_font "default" in
    let fd = 
      Fonts.merge (Fonts.merge !Fonts.default attrs_default) attrs_fixed in
    let (_, opts) = Fonts.compute_tag fd in
    Text.configure text opts;
    tw <- text;

    let buffer = String.create 2048 in
    let red = ref 0 in
    let size = 
      try Some (Http_headers.contentlength dh.document_headers)
      with Not_found -> None (* duh *) 
    in

    let lastwascr = ref false in
    dh.document_feed.feed_schedule
      (fun () ->
         try let n = dh.document_feed.feed_read buffer 0 2048 in
         if n = 0 then begin
           if !lastwascr 
           then self#add_text "\n";
           self#add_text ""; (* special case to indicate end *)
           self#set_progress (Some !red) !red;
           self#finish false
         end else begin
           red := !red + n;
           self#set_progress size !red;
           let s,flag = Mstring.norm_crlf !lastwascr buffer 0 n in
           lastwascr := flag;
           self#add_text s
         end
     with Unix_error(_,_,_) ->
         self#set_progress size (-1);
         self#finish true
     );

  method di_abort = 
    self#finish true
  method di_destroy = 
    if Winfo.exists frame 
    then destroy frame
  method di_redisplay = 
    self#redisplay
  method di_title =
    Url.string_of dh.document_id.document_url
  method di_load_images = ()
  method di_fragment f = ()
  method di_update = ()

  (*s: [[Plain.plain]] other methods or fields *)
  method di_source = ()
  (*e: [[Plain.plain]] other methods or fields *)
end
(*e: class Plain.plain *)

(*s: function Plain.display_plain *)
(* Viewing text/plain *)

let display_plain _mediapars top vcontext dh =
  let viewer = new plain (top,vcontext,dh) in
  viewer#init;
  Some (viewer :> Viewers.display_info)
(*e: function Plain.display_plain *)

(*s: toplevel Plain._1 *)
let _ =
  Viewers.add_builtin ("text","plain") display_plain
(*e: toplevel Plain._1 *)
(*e: ./viewers/plain.ml *)

(*s: ./viewers/plain.ml *)
open Tk
open Unix
open Frx_text
open Document
open Viewers
open Feed

(*s: class Plain.plain *)
class display_plain ((top : Widget.widget),
                     (ctx : Viewers.context),
                     (dh : Document.handle)) =
 object (self)
  inherit Viewers.display_info () as di  (* gives us basic features *)
(*
  inherit Htmlw.viewer_globs (ctx, dh)
*)
  method ctx = ctx

  method di_title =
    Url.string_of dh.document_id.document_url

  (*s: [[Plain.plain]] private fields *)
  (* text widget, will be set to the proper value in init() *)
  val mutable (*private*) tw = Widget.default_toplevel
  (*x: [[Plain.plain]] private fields *)
  val mutable (*private*) terminated = false
  (*e: [[Plain.plain]] private fields *)

  (*s: [[Plain.plain]] frame widget methods *)
  val frame = 
    if not (Winfo.exists top) 
    then failwith "too late"
    else Frame.create top [Class "Plain"]
  method frame = frame
  method di_widget = frame
  (*e: [[Plain.plain]] frame widget methods *)
  (*s: [[Plain.plain]] init method *)
  method init =
    Log.debug "Plain#init";
    (*s: [[Plain.plain#init]] set header widgets *)
    (*
    let hgbas, progf = Htmlw.progress_report frame ctx in
    set_progress <- progf;
    pack [hgbas] [Side Side_Bottom; Fill Fill_X];
    let (headgroup,_,_,_,_) = 
      Htmlw.html_head_ui dh.document_headers (fun () -> ()) (ref false)
       frame ctx 
    in 
    pack [headgroup][Side Side_Top; Fill Fill_X];
    *)
    (*e: [[Plain.plain#init]] set header widgets *)

    (* Scrollable text widget *)
    let hgroup = Frame.create_named frame "textw" [Class "Plain"] in
    let ftext, text = 
      Frx_text.new_scrollable_text hgroup [Wrap WrapWord; State Disabled] true 
    in
    (*s: [[Plain.plain#init]] tk fixes on text widget *)
    (* Tk4.0pl3 fix, + avoid cb to scrollbar *)
    Text.configure text [TakeFocus true; InsertOffTime 0];
    Frx_text.addsearch text;
    (*e: [[Plain.plain#init]] tk fixes on text widget *)
    (* IN THIS ORDER -- RESIZING *)
    pack [ftext][Side Side_Left; Fill Fill_Both; Expand true];
    pack [hgroup][Fill Fill_Both; Expand true];

    (*s: [[Plain.plain#init]] setup fonts *)
    (*
    (* pick up the fixed font *)
    let attrs_fixed   = Styles.get_font "fixed" in
    let attrs_default = Styles.get_font "default" in
    let fd = 
      Fonts.merge (Fonts.merge !Fonts.default attrs_default) attrs_fixed in
    let (_, opts) = Fonts.compute_tag fd in
    Text.configure text opts;
    *)
    (*e: [[Plain.plain#init]] setup fonts *)

    tw <- text;

    (*s: [[Plain.plain#init]] locals *)
    let buffer = String.create 2048 in
    let size = 
      try Some (Http_headers.contentlength dh.dh_headers)
      with Not_found -> None (* duh *) 
    in
    let read = ref 0 in
    let lastwascr = ref false in
    (*e: [[Plain.plain#init]] locals *)
    dh.document_feed.feed_schedule (fun () ->
      (*s: [[Plain.plain#init]] feed schedule callback *)
      try 
        let n = dh.document_feed.feed_read buffer 0 2048 in
        if n = 0 then begin
          if !lastwascr 
          then self#add_text "\n";
          self#add_text ""; (* special case to indicate end *)
          self#set_progress (Some !read) !read;
          self#finish false (* not abort *)
        end else begin
          read := !read + n;
          self#set_progress size !read;
          let s,flag = Mstring.norm_crlf !lastwascr buffer 0 n in
          lastwascr := flag;
          self#add_text s
        end
      with Unix_error(_,_,_) ->
         self#set_progress size (-1);
         self#di_abort
      (*e: [[Plain.plain#init]] feed schedule callback *)
     );
  (*e: [[Plain.plain]] init method *)
  (*s: [[Plain.plain]] adding text method *)
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
  (*e: [[Plain.plain]] adding text method *)

  (*s: [[Plain.plain]] progress methods *)
  (* progress report *)
  val mutable set_progress = Progress.no_meter
  method set_progress = set_progress
  (*e: [[Plain.plain]] progress methods *)
  (*s: [[Plain.plain]] abort methods *)
  method di_abort = 
    self#finish true

  (* [finish abort?] *)
  method finish abort =
    if not terminated then begin
      terminated <- true;
      self#ctx#log (if abort then "Aborted" else "");
      Document.dclose true dh;
    end
  (*e: [[Plain.plain]] abort methods *)
  (*s: [[Plain.plain]] redisplay methods *)
  method di_redisplay = 
    self#redisplay

  (* to redisplay, we have to destroy all widgets, then restart, except
     that we don't use the feed, but rather the cache *)
  method redisplay =
    failwith "redisplay:TODO"
    (*
    try
      dh <- Decoders.insert (Cache.renew_handle dh);
      Winfo.children frame |> List.iter destroy;
      self#init
    with Not_found ->
      Error.f (s_ "Document not in cache anymore")
    *)
  (*e: [[Plain.plain]] redisplay methods *)
  (*s: [[Plain.plain]] destroy methods *)
  method di_destroy = 
    if Winfo.exists frame 
    then Tk.destroy frame
  (*e: [[Plain.plain]] destroy methods *)
  (*s: [[Plain.plain]] empty methods *)
  method di_load_images = ()
  (*x: [[Plain.plain]] empty methods *)
  method di_update = ()
  (*x: [[Plain.plain]] empty methods *)
  method di_fragment _frag = 
    ()
  (*e: [[Plain.plain]] empty methods *)
  (*s: [[Plain.plain]] other methods or fields *)
  method di_source = ()
  (*e: [[Plain.plain]] other methods or fields *)
end
(*e: class Plain.plain *)

(*s: function Plain.display_plain *)
(* Viewing text/plain *)

let display_plain _mediapars top vcontext dh =
  let viewer = new display_plain (top,vcontext,dh) in
  viewer#init;
  Some (viewer :> Viewers.display_info)
(*e: function Plain.display_plain *)

(*s: toplevel Plain._1 *)
let _ =
  Viewers.add_builtin ("text","plain") display_plain
(*e: toplevel Plain._1 *)
(*e: ./viewers/plain.ml *)

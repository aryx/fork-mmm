(*s: ./display/imgload.ml *)
open Printf
open Tk
open Tkanim
open Www
open Uri
open Hyper
open Maps
open Embed
open Img
open Viewers

(*s: type Imgload.mode (./display/imgload.ml) *)
(* Images are embedded objects, with a twist *)
type mode =
    DuringDoc
  | AfterDocAuto
  | AfterDocManual
(*e: type Imgload.mode (./display/imgload.ml) *)

(*s: constant Imgload.mode *)
(* Preference settings *)
let mode = ref AfterDocManual
(*e: constant Imgload.mode *)
(*s: constant Imgload.no_images *)
let no_images = ref false
(*e: constant Imgload.no_images *)
(*s: constant Imgload.gif_anim_auto *)
let gif_anim_auto = ref false
(*e: constant Imgload.gif_anim_auto *)

(*s: function Imgload.display *)
(* Utilities *)
let display emb i =
  let prop = ref false in
  begin
  if Winfo.exists emb.embed_frame then
  match emb.embed_map with
    ClientSide hlink -> (* kill'em all *)
      begin try
    List.iter destroy (Winfo.children emb.embed_frame);
    let w, h =
     match i with
       Still x -> 
         begin match x with
           ImageBitmap i -> Imagebitmap.width i, Imagebitmap.height i
         | ImagePhoto  i -> Imagephoto.width i, Imagephoto.height i
         | _ -> failwith "invalid image" 
         end
     | Animated anm -> Tkanim.width anm, Tkanim.height anm in
    let c = Canvas.create emb.embed_frame 
           [Width (Pixels w); Height (Pixels h)] in
    prop := true; (*fit the size to the image*)
    Tk.bindtags c ((WidgetBindings emb.embed_frame)::Tk.bindtags_get c);
      pack [c][];
    let ii = Canvas.create_image c (Pixels 0) (Pixels 0)
                   [Anchor NW] in
    begin
      match i with
        Still x -> Canvas.configure_image c ii [x]
      | Animated anm -> 
          begin
        let f = Tkanim.animate_canvas_item c ii anm in
        (* binding on c is bad... *)
        bind c (Glevents.get "stopanim") (BindSet ([], (fun _ ->
          f false)));
        bind c (Glevents.get "restartanim") (BindSet ([], (fun _ ->
          f true)));
        (* I am sure it doesn't work *)
        Canvas.configure c [Cursor (XCursor "watch")];
        if !gif_anim_auto then f false
          end
    end;
    (* now we have the image displayed in a canvas.
       and we can create the client side map *)
    let uri = Hyper.resolve hlink in
    let name =
      match uri.uri_fragment with 
        None -> uri.uri_url
      | Some frag -> sprintf "%s#%s" uri.uri_url frag
    in
      match Maps.get name with
        KnownMap m -> Cmap.gfx_mode emb m c
      | RequestedMap event ->
         Frx_synth.bind c event 
         (fun c -> 
            match Maps.get name with
              RequestedMap _ ->
             Log.f "INTERNAL ERROR: delayed_client_side"
            | KnownMap m -> Cmap.gfx_mode emb m c)
      with
    e -> Log.f (sprintf "INTERNAL ERROR in display (%s)"
                    (Printexc.to_string e))
      end

  | _ ->
    (* in all other cases, get the alt label, and configure it *)
    (* WARNING: there may be an hypermenu here *)
    List.iter (function w ->
      match Winfo.class_name w with
    "Label" ->
    (* remove its border *)
    Label.configure w [BorderWidth (Pixels 0)];
    begin match i with
      Still x -> 
        Label.configure w [x];
        begin match x with
          ImageBitmap _ | ImagePhoto _ ->
        prop := true; (*fit the size to the image*)
        | _ -> () (*We cannot restore the origianl size of the window...*)
        end;
        (* Utility to copy the img url in the selection buffer *)
        bind w (Glevents.get "copyimgurl")
          (BindSet ([], (fun _ ->
        emb.embed_context#invoke "copy" emb.embed_hlink)));
        (* Updating an image *)
        begin
          try 
        let url = 
          Lexurl.make (Hyper.resolve emb.embed_hlink).uri_url in
         bind w (Glevents.get "updateimage")
          (BindSet ([], (fun _ -> Img.update url)))
          with
        Url.Url_Lexing _ -> ()
        end

    | Animated anm -> 
        begin
          let f = Tkanim.animate w anm in
          bind w (Glevents.get "stopanim") (BindSet ([], (fun _ ->
        f false)));
          bind w (Glevents.get "restartanim") (BindSet ([], (fun _ ->
        f true)));
        Label.configure w [Cursor (XCursor "watch")];
          if !gif_anim_auto then f false;
          prop := true (*fit the size to the image*)
        end
    end
      |	"Canvas" -> destroy w (* delete the progress meter *)
      |	_ -> ())
      (Winfo.children emb.embed_frame)
  end;
  if !prop then Pack.propagate_set emb.embed_frame true
(*e: function Imgload.display *)

(*s: function Imgload.put_alt *)
(* put up the alternate text *)
let put_alt emb =
  let m = Label.create_named emb.embed_frame "alt" [Text emb.embed_alt] in
  (* make sure all bindings we put on the frame are attached there *)
  Tk.bindtags m ((WidgetBindings emb.embed_frame)::Tk.bindtags_get m);
  pack [m][Fill Fill_Both; Expand true];
  if not (Pack.propagate_get emb.embed_frame) then begin (* with width and height *)
    if Winfo.reqwidth emb.embed_frame < Winfo.reqwidth m then
      Frame.configure emb.embed_frame [Width (Pixels (Winfo.reqwidth m))];
    if Winfo.reqheight emb.embed_frame < Winfo.reqheight m then
      Frame.configure emb.embed_frame [Height (Pixels (Winfo.reqheight m))];
(* Buggy
    let wf = Winfo.reqwidth emb.embed_frame 
    and wm = Winfo.reqwidth m
    and hf = Winfo.reqheight emb.embed_frame 
    and hm = Winfo.reqheight m
    in
    if wf < wm || hf < hm then begin
      bind emb.embed_frame [[], Enter] (BindExtend ([], (fun _ ->
    if wf < wm then
      Frame.configure emb.embed_frame [Width (Pixels wm)];
    if hf < hm then
      Frame.configure emb.embed_frame [Height (Pixels hm)])));
      bind emb.embed_frame [[], Leave] (BindExtend ([], (fun _ ->
    Frame.configure emb.embed_frame [Width (Pixels wf)];
    Frame.configure emb.embed_frame [Height (Pixels hf)])))
    end
*)
  end
(*e: function Imgload.put_alt *)

(*s: function Imgload.make_auto *)
(* for delayed load, add binding *)
let make_auto delayed emb =
  try
    let url = (Www.make emb.embed_hlink).www_url in
    bind emb.embed_frame
      (Glevents.get "loadimage")
      (BindSet ([], (fun _ -> Img.ImageScheduler.flush_one delayed url)))
  with
    e -> Log.f (sprintf "Can't compute image link (%s)" (Printexc.to_string e))
(*e: function Imgload.make_auto *)

(* for manual load, add binding
let make_manual emb =
  try
    let url = (Www.make emb.embed_hlink).www_url in
    bind emb.embed_frame
      (Glevents.get "loadimage")
      (BindSet ([], (fun _ -> activate emb)))
  with
    e -> Log.f (sprintf "Can't compute image link (%s)" (Printexc.to_string e))
 *)

(*s: function Imgload.make_map *)
(* If the object is clickable, make it visible *)

let make_map emb =
  let visible = [
    BorderWidth (Pixels (Tkresource.int "clickableBorderWidth" 2)); 
    Relief (Tkresource.relief "clickableRelief" Raised); 
    Cursor (XCursor (Tkresource.string "clickableCursor" "hand2"));
    Background (NamedColor (Tkresource.string "clickableBackground" "white"))]
  and visible_map = [
    BorderWidth (Pixels (Tkresource.int "clickableBorderWidth" 2)); 
    Relief (Tkresource.relief "clickableRelief" Raised); 
    Cursor (XCursor (Tkresource.string "clickableMapCursor" "left_ptr"));
    Background (NamedColor (Tkresource.string "clickableBackground" "white"))]
  in
  let reconfigure_frame f =
    let internal_width = try int_of_string (cget f CWidth) with _ -> 0 
    and internal_height = try int_of_string (cget f CHeight) with _ -> 0
    and border_width = try int_of_string (cget f CBorderWidth) with _ -> 0
    in
    if internal_width = 0 || internal_height = 0 then ()
    else
      Frame.configure f [ Width (Pixels (internal_width + border_width * 2));
              Height (Pixels (internal_height + border_width * 2))]
  in
  match emb.embed_map with
    ClientSide hlink ->
      Frame.configure emb.embed_frame visible_map;
      reconfigure_frame emb.embed_frame; 
     (* At this moment, we assume that we are in alt mode.
    If the image gets loaded, the label gets destroyed and
    the callback will never be invoked. Instead, it will
    be called from "display" *)
      begin try
       match Winfo.children emb.embed_frame with
    [l] when Winfo.class_name l = "Label" ->
      let uri = Hyper.resolve hlink in
      let name =
        match uri.uri_fragment with 
          None -> uri.uri_url
        | Some frag -> sprintf "%s#%s" uri.uri_url frag in
      begin match Maps.get name with
        KnownMap m -> Cmap.alt_mode emb m l
      | RequestedMap event ->
         Frx_synth.bind l event 
         (fun l -> 
            match Maps.get name with
              RequestedMap _ -> Log.f 
             "INTERNAL ERROR: delayed_client_side"
            | KnownMap m -> Cmap.alt_mode emb m l)
      end
       | _ -> Log.f "make_map. children not a label"
      with
    _ -> ()
      end
  | ServerSide link ->
      Frame.configure emb.embed_frame visible;
      reconfigure_frame emb.embed_frame; 
      (new Htbind.servermap (emb.embed_frame, link))#init emb.embed_context
  | Direct link -> 
      Frame.configure emb.embed_frame visible;
      reconfigure_frame emb.embed_frame; 
      (new Htbind.directmap (emb.embed_frame, link))#init emb.embed_context
  | NoMap -> ()
  | FormMap getlink ->
      Frame.configure emb.embed_frame visible;
      reconfigure_frame emb.embed_frame; 
      (new Htbind.formmap (emb.embed_frame, getlink))#init emb.embed_context
(*e: function Imgload.make_map *)


(* The default behavior *)
class loader () =
 object (self)
  val mutable loaded = UrlSet.empty

  method private add_loaded url = 
    loaded <- UrlSet.add url loaded

  method add_image emb =
    put_alt emb; (* make the alt widget*)
    make_map emb (* and possible bindings *)

  method private activate emb =
    Log.debug "Activating image";
    try
      Img.get emb.embed_context#base
    emb.embed_hlink
        (fun url i -> 
      display emb i;
      self#add_loaded url)
        (Tk_progress.meter emb.embed_frame)
    with
      e -> Log.f (sprintf "Can't load image (%s)" (Printexc.to_string e))

  method flush_images = ()
  method load_images = ()
  method update_images = 
    UrlSet.iter Img.update loaded
end

class synchronous () =
 object
  inherit loader () as super

  method add_image emb =
    super#add_image emb; super#activate emb
end

class auto () =
 object (self)
  inherit loader () as super
  val q = ImageScheduler.new_delayed()

  method add_image emb =
     super#add_image emb;
     try
       let wr = Www.make emb.embed_hlink in
       wr.www_headers <- "Accept: image/*" :: wr.www_headers;
       ImageScheduler.add_delayed q
     wr
     emb.embed_context#base
     (fun url i -> display emb i; self#add_loaded url)
     (Tk_progress.meter emb.embed_frame)
     with
       e -> Log.f (sprintf "Can't compute image link (%s)"
                   (Printexc.to_string e))

  method flush_images = ImageScheduler.flush_delayed q
end

class manual () =
 object
  inherit auto () as super

  method add_image emb =
    super#add_image emb;
    make_auto q emb
 
  method flush_images = ()

  method load_images = ImageScheduler.flush_delayed q
end

(*s: function Imgload.create *)
let create () =
   if !no_images then new loader ()
   else match !mode with
     DuringDoc -> (new synchronous () :> loader)
   | AfterDocAuto -> (new auto() :> loader)
   | AfterDocManual -> (new manual() :> loader)
(*e: function Imgload.create *)

(*e: ./display/imgload.ml *)

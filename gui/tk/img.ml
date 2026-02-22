(*s: retrieve/img.ml *)
(* Image cache and scheduled image downloading *)

open Fpath_.Operators
open Tk

(*s: constant [[Img.gif_anim_load]] *)
(* Images are a special case of embedded data, because Tk caches them
   internally. Thus, we attempt to maintain our own cache logic above
   Tk's one 
 *)

let gif_anim_load = ref false
(*e: constant [[Img.gif_anim_load]] *)

module ImageData =
  struct
    
    type t = Tkanim.imageType

    let gamma = ref 1.0
    let jpeg_converter = ref "djpeg"
    let verbose = ref false

   (* 
    * The image cache
    *)

    let set_of_list l = 
      List.fold_right Document.DocumentIDSet.add l Document.DocumentIDSet.empty

    (* url -> (option for tk configure, set of referers, headers) *)
    let img_cache = 
       (Hashtbl.create 53 : (Url.t, 
                 Tkanim.imageType * Document.DocumentIDSet.t ref
                   * string list) Hashtbl.t)

    (* Debugging *)
    let dump () =
      Hashtbl.iter (fun url (_,r, _) ->
       Logs.debug (fun m -> m "IMG %s" (Url.string_of url));
       Document.DocumentIDSet.iter 
         (fun did -> Logs.debug (fun m -> m "\tref: %s"
                         (Url.string_of did.document_url)))
         !r)
    img_cache

    let add url imgdesc referers headers =
      Hashtbl.add img_cache url (imgdesc, ref (set_of_list referers), headers)

    (* Raises Not_found *)
    let cache_access url from =
      let img, refs, _ = Hashtbl.find img_cache url in
    refs := Document.DocumentIDSet.add from !refs;
    img

    let direct_cache_access  = Hashtbl.find img_cache

    (* Delete an image from the cache *)
    let delete_image img =
      if !verbose 
      then Logs.info (fun m -> m "Removing img %s" (Url.string_of img));
      match Hashtbl.find img_cache img with
    Still x, _, _ ->
      begin match x with
        Bitmap _ -> ()
      | ImageBitmap n ->
          Imagebitmap.delete n; Hashtbl.remove img_cache img
      | ImagePhoto n ->
          Imagephoto.delete n; Hashtbl.remove img_cache img
      | _ -> assert false
      end
      |	Animated anm, _, _ -> Tkanim.delete anm; Hashtbl.remove img_cache img

    (* Remove reference to an image, clean *)
    let remove_reference (referer : Document.id) =
      if !verbose 
      then Logs.info (fun m -> m "Removing img references from %s(%d)" 
            (Url.string_of referer.document_url)
             referer.document_stamp);
      let delete_them = ref [] in
      Hashtbl.iter
    (fun img (_o, refs, _) ->
        refs := Document.DocumentIDSet.remove referer !refs;
        if Document.DocumentIDSet.is_empty !refs then
         delete_them := img :: !delete_them)
    img_cache;
      List.iter delete_image !delete_them

    let broken_data = Tkanim.Still (Bitmap (Predefined "error"))

    (* load an image *)
    (* For GIFs, we use JPF's Tkanim package first *)
    let tk_load_gif file =
      try
    if !gif_anim_load then Tkanim.create file
    else Still (ImagePhoto (Imagephoto.create [File file; Gamma !gamma]))
      with Protocol.TkError _ -> broken_data

    (* For JPEG, we attempt internal load first, because we might have
       an extension for loading them *)
    let tk_load_jpeg file =
      try Tkanim.Still (ImagePhoto (Imagephoto.create [File file; Gamma !gamma]))
      with Protocol.TkError _ ->
    let pnmfile = Msys.mktemp "pnm" in
    let cmd = (!jpeg_converter^" "^file^" > "^pnmfile) in
    try match Sys.command cmd with
      0 ->
        let img = Tkanim.Still (ImagePhoto (Imagephoto.create
                     [File pnmfile; Gamma !gamma])) in
        Msys.rm pnmfile;
        img
    | _ -> Msys.rm pnmfile; broken_data
    with
      Protocol.TkError _ ->
        Msys.rm pnmfile;
        Still (Bitmap (Predefined "question"))

    (* other formats *)
    let tk_load_other file =
      Tkanim.Still (
        try ImageBitmap (Imagebitmap.create [File file])
    with
      Protocol.TkError _ ->
        try ImagePhoto (Imagephoto.create [File file; Gamma !gamma])
        with
         Protocol.TkError _ -> Bitmap (Predefined "question"))

    let load dh referers (file : Fpath.t) =
      Retype.f dh;
      match dh.document_status with
    200 ->
      let url = dh.document_id.document_url in
      let img = 
        try
          let ctype = Http_headers.contenttype dh.dh_headers in
          match Lexheaders.media_type ctype with
           ("image","jpeg"), _ -> Low.busy tk_load_jpeg !!file
          | ("image","gif"), _ -> Low.busy tk_load_gif !!file
          | _,_ -> Low.busy tk_load_other !!file
        with
        | Not_found -> Low.busy tk_load_other !!file 
        | Http_headers.Invalid_header _ -> Msys.rm !!file; broken_data
          in
      if !verbose 
      then Logs.info (fun m -> m "Loaded %s as %s" !!file (Url.string_of url));
      Msys.rm !!file;
      add url img referers dh.dh_headers;
      img
      |	304 -> (* we did an update an a document, and it induced a 
          recursive update. The document didn't change *)
      begin try 
        Msys.rm !!file;
        cache_access dh.document_id.document_url (List.hd referers)
      with
        Not_found -> broken_data
      end
        
      | _ -> (* other cases *)
      Msys.rm !!file; broken_data
      
      (* error during img downloading *)
    let error url job =
      Logs.err (fun m -> m "Could not load image at %s" (Url.string_of url));
      let img = Tkanim.Still (Bitmap (Predefined "error")) in
      add url img (List.map fst job) [];
      List.iter (fun (_, (cont,_)) -> cont url img) job
    
      (* Invalid urls in images are silently ignored *)
    let error_msg ((w : Www.request), msg) = 
      Logs.err (fun m -> m "Invalid image request: %s (%s)" 
           (Url.string_of w.www_url) msg);
      
  end
    

module ImageScheduler = Scheduler.Make(ImageData)


(*s: toplevel [[Img._1]] *)
(* Advertise ourselfs to the internal cache *)
let _ =
 Cache.cutlinks := ImageData.remove_reference :: !Cache.cutlinks
(*e: toplevel [[Img._1]] *)

(*s: function [[Img.get]] *)
(* ??? -> <> *)
let get (caps : < Cap.network; ..>) (did : Document.id) (link : Hyper.link) cont 
        (prog : Scheduler.progress_func) : unit =
  let wr = Www.make link in
  wr.www_headers <- "Accept: image/*" :: wr.www_headers;
  ImageScheduler.add_request (caps :> < Cap.network >) wr did cont prog
(*e: function [[Img.get]] *)

(*s: function [[Img.update]] *)
(* ??? -> <> *)
let update (caps : < Cap.network; ..>) (url : Url.t) : unit =
  try
    let (oldi, refs, headers) = ImageData.direct_cache_access url in
    let link = Hyper.default_link (Url.string_of url) in
    let wr = Www.make link in
    let date_received = Http_headers.get_header "date" headers in
    wr.www_headers <- 
       ("If-Modified-Since: "^date_received)
       :: "Pragma: no-cache"
       :: wr.www_headers;

    ImageScheduler.add_request (caps :> < Cap.network >) wr (Document.DocumentIDSet.choose !refs)
      (fun _url i -> 
        match oldi, i with
        | Still (ImagePhoto oldn) , Still (ImagePhoto newn) ->
          Imagephoto.copy oldn newn []
        | _, _ -> ()
      )
      Progress.no_meter

  with
    Not_found ->  (* either not in cache (bogus) or no date *)
      ()
(*e: function [[Img.update]] *)
(*e: retrieve/img.ml *)

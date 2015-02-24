(*s: ./retrieve/img.ml *)
(* Image cache and scheduled image downloading *)
open Printf
open Unix
open Tk
open Tkanim
open Mstring
open Document
open Www
open Hyper
open Url
open Http_headers

(*s: constant Img.gif_anim_load *)
(* Images are a special case of embedded data, because Tk caches them
   internally. Thus, we attempt to maintain our own cache logic above
   Tk's one 
 *)

let gif_anim_load = ref false
(*e: constant Img.gif_anim_load *)

module ImageData =
  struct
    
    type t = Tkanim.imageType

    let gamma = ref 1.0
    let jpeg_converter = ref "djpeg"
    let verbose = ref false

   (* 
    * The image cache
    *)


    let set_of_list l = List.fold_right DocumentIDSet.add l DocumentIDSet.empty

    (* url -> (option for tk configure, set of referers, headers) *)
    let img_cache = 
       (Hashtbl.create 53 : (Url.t, 
                 Tkanim.imageType * DocumentIDSet.t ref
                   * string list) Hashtbl.t)

    (* Debugging *)
    let dump () =
      Hashtbl.iter (fun url (_,r, _) ->
      	Log.f (sprintf "IMG %s" (Url.string_of url));
    DocumentIDSet.iter 
      	  (fun did -> Log.f (sprintf "\tref: %s"
                         (Url.string_of did.document_url)))
      	  !r)
    img_cache

    let add url imgdesc referers headers =
      Hashtbl.add img_cache url (imgdesc, ref (set_of_list referers), headers)

    (* Raises Not_found *)
    let cache_access url from =
      let img, refs, _ = Hashtbl.find img_cache url in
    refs := DocumentIDSet.add from !refs;
    img

    let direct_cache_access  = Hashtbl.find img_cache

    (* Delete an image from the cache *)
    let delete_image img =
      if !verbose then Log.f (sprintf "Removing img %s" (Url.string_of img));
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
    let remove_reference referer =
      if !verbose then 
     Log.f (sprintf "Removing img references from %s(%d)" 
            (Url.string_of referer.document_url)
             referer.document_stamp);
      let delete_them = ref [] in
      Hashtbl.iter
    (fun img (o, refs, _) ->
        refs := DocumentIDSet.remove referer !refs;
        if DocumentIDSet.is_empty !refs then
         delete_them := img :: !delete_them)
    img_cache;
      List.iter delete_image !delete_them

    let broken_data = Still (Bitmap (Predefined "error"))

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
      try Still (ImagePhoto (Imagephoto.create [File file; Gamma !gamma]))
      with Protocol.TkError _ ->
    let pnmfile = Msys.mktemp "pnm" in
    let cmd = (!jpeg_converter^" "^file^" > "^pnmfile) in
    try match Sys.command cmd with
      0 ->
        let img = Still (ImagePhoto (Imagephoto.create
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
      Still (
        try ImageBitmap (Imagebitmap.create [File file])
    with
      Protocol.TkError _ ->
        try ImagePhoto (Imagephoto.create [File file; Gamma !gamma])
        with
         Protocol.TkError _ -> Bitmap (Predefined "question"))

    let load dh referers file =
      Retype.f dh;
      match dh.document_status with
    200 ->
      let url = dh.document_id.document_url in
      let img = 
        try
          let ctype = contenttype dh.document_headers in
          match Lexheaders.media_type ctype with
          	("image","jpeg"), _ -> Low.busy tk_load_jpeg file
          | ("image","gif"), _ -> Low.busy tk_load_gif file
          | _,_ -> Low.busy tk_load_other file
        with
        | Not_found -> Low.busy tk_load_other file 
        | Invalid_HTTP_header _ -> Msys.rm file; broken_data
          in
      if !verbose then
        Log.f (sprintf "Loaded %s as %s" file (Url.string_of url));
      Msys.rm file;
      add url img referers dh.document_headers;
      img
      |	304 -> (* we did an update an a document, and it induced a 
          recursive update. The document didn't change *)
      begin try 
        Msys.rm file;
        cache_access dh.document_id.document_url (List.hd referers)
      with
        Not_found -> broken_data
      end
        
      | _ -> (* other cases *)
      Msys.rm file; broken_data
      
      (* error during img downloading *)
    let error url job =
      Log.f (sprintf "Could not load image at %s" (Url.string_of url));
      let img = Still (Bitmap (Predefined "error")) in
      add url img (List.map fst job) [];
      List.iter (fun (_, (cont,_)) -> cont url img) job
    
      (* Invalid urls in images are silently ignored *)
    let error_msg (w, msg) = 
      Log.f (sprintf "Invalid image request: %s (%s)" 
           (Url.string_of w.www_url) msg);
      
  end
    

module ImageScheduler = Scheduler.Make(ImageData)


(*s: toplevel Img._1 *)
(* Advertise ourselfs to the internal cache *)
let _ =
 Cache.cutlinks := ImageData.remove_reference :: !Cache.cutlinks
(*e: toplevel Img._1 *)

(*s: function Img.get *)
let get did link cont prog =
  let wr = Www.make link in
   wr.www_headers <- "Accept: image/*" :: wr.www_headers;
   ImageScheduler.add_request wr did cont prog
(*e: function Img.get *)

(*s: function Img.update *)
let update url =
  try
    let (oldi,refs,headers) = ImageData.direct_cache_access url in
    let link = { h_uri = Url.string_of url;
         h_context = None;
         h_method = GET;
             h_params = []} in
    let wr = Www.make link in
    let date_received = get_header "date" headers in
    wr.www_headers <- 
       ("If-Modified-Since: "^date_received)
       :: "Pragma: no-cache"
       :: wr.www_headers;

    ImageScheduler.add_request wr (DocumentIDSet.choose !refs)
      (fun url i -> 
    match oldi, i with
      Still (ImagePhoto oldn) , Still (ImagePhoto newn) ->
        Imagephoto.copy oldn newn []
 	| _, _ -> ())
      Progress.no_meter

  with
    Not_found ->  (* either not in cache (bogus) or no date *)
      ()
(*e: function Img.update *)

(*e: ./retrieve/img.ml *)

(*s: viewers/embed.ml *)
(* Embedded documents *)
open I18n
open Tk
open Document
open Www
open Hyper

open Http_headers

(* Assume any kind of data could be embedded 
 * The normal retrieval, used by the scheduler, makes its own decision
 * about the need to cache the document (basically, it caches html and text)
 * Thus, we want to decide here if we want to cache documents retrieved
 * via Embed.
 *)
module EmbeddedData =
  struct

    type t = document

    let cache_access url _referer =
      let did =  {document_url = url; document_stamp = no_stamp} in
      (* look in the cache *)
      Cache.find did

    (* The document is here in the file. Either it's been cached, and
       then we just get its cache value, or we add it to the cache 
       dh is closed; we use only the headers
       NOTE: if we are updating over an old version, fix the cache
     *)
    let load dh referers file =
      Retype.f dh;
      match dh.document_status with
    200 ->
      begin try 
        let doc = Cache.find dh.document_id in
        let this_date = get_header "date" dh.dh_headers in
        let cache_date = get_header "date" doc.document_headers in
        if this_date <> cache_date then raise Not_found
        else doc
      with
        Not_found ->
          let doc = { document_address = dh.document_id.document_url;
                 document_data = FileData (file, true);
                 document_headers = dh.dh_headers} in
          Cache.add dh.document_id doc;
          Cache.finished dh.document_id;
          doc
      end
      |	304 -> (* return the previous version *)
      begin try 
        Msys.rm file;
        cache_access dh.document_id.document_url (List.hd referers)
      with
        Not_found -> failwith "load"
      end

      |	_ -> failwith "load"

    let error url _jobs = 
      Error.f (s_ "Can't find embedded document %s"  (Url.string_of url))

    let error_msg (_,_) = ()
  end


(* The embedded data scheduler *)
module EmbeddedScheduler = Scheduler.Make(EmbeddedData)


(*s: constant [[Embed.embedded_viewers]] *)
(* Embedded viewers *)

let embedded_viewers = Hashtbl.create 11
(*e: constant [[Embed.embedded_viewers]] *)
let add_viewer = Hashtbl.add embedded_viewers 
and rem_viewer = Hashtbl.remove embedded_viewers


(*s: function [[Embed.embedded_viewer]] *)
let embedded_viewer frame ctx doc =
  (* Destroy the alt window *)
  List.iter Tk.destroy (Winfo.children frame);
  try
    let ctype = contenttype doc.document_headers in
    let (typ,subtyp),l = Lexheaders.media_type ctype in
    try
      let viewer = 
    try Hashtbl.find embedded_viewers (typ,subtyp)
        with Not_found -> Hashtbl.find embedded_viewers (typ,"*") in
      viewer l frame ctx doc
    with
    Not_found ->
      let t = 
        s_ "Embed Error: no viewer for type %s/%s" typ subtyp in
      let l = Label.create frame [Text t] in pack [l][]
  with
    Not_found ->
      let t = s_ "Embed Error: no type for document %s" 
                          (Url.string_of doc.document_address) in 
      let l = Label.create frame [Text t] in pack [l][]
  | Invalid_HTTP_header e ->
      let t = 
       s_ "Embed Error: malformed type %s (%s)"
         (contenttype doc.document_headers) e in
      let l = Label.create frame [Text t] in pack [l][]
(*e: function [[Embed.embedded_viewer]] *)


(*s: type [[Embed.embobject]] *)
(* Embedded objects *)
type embobject = {
  embed_hlink : Hyper.link;               (* hyperlink to the object *)
  embed_frame : Widget.widget;  
     (* the frame where the viewers can do their stuff *)
  embed_context : Viewers.context;
  embed_map : Maps.t;                  (* associated map *)
  embed_alt : string
 }
(*e: type [[Embed.embobject]] *)

(*s: constant [[Embed.embedded]] *)
(* Remember all current embedded objects by their frame *)
let embedded = (Hashtbl.create 101 : (string, embobject) Hashtbl.t)
(*e: constant [[Embed.embedded]] *)

(*s: function [[Embed.add_embed]] *)
(* add and notify *)
let add_embed emb = 
  Hashtbl.add embedded (Widget.name emb.embed_frame) emb;
  Frx_synth.send "setembed" emb.embed_frame
(*e: function [[Embed.add_embed]] *)

(*s: function [[Embed.when_destroyed]] *)
(* when the frame gets destroyed, remove us from the table *)
let when_destroyed w =
  Hashtbl.remove embedded (Widget.name w)
(*e: function [[Embed.when_destroyed]] *)
(*s: toplevel [[Embed._1]] *)
let _ =
  Protocol.add_destroy_hook when_destroyed
(*e: toplevel [[Embed._1]] *)

(*s: function [[Embed.add]] *)
(* Queueing an embed *)
let add ({ embed_hlink = link;
       embed_frame = frame;
       embed_context = embed_ctx;
       embed_map = _m;
       embed_alt = alt_txt} as emb) =
  (* Put up the ALT text *)
  List.iter Tk.destroy (Winfo.children frame);
  pack [Label.create_named frame "alt" [Text alt_txt]][];
  (* Check if the type is defined and a viewer available *)
  try
   let given_type = List.assoc "type" embed_ctx#params in
   let ((typ,subtyp), parms) = Lexheaders.media_type given_type in
   try
     let viewer = 
       try Hashtbl.find embedded_viewers (typ,subtyp)
       with Not_found -> Hashtbl.find embedded_viewers (typ, "*")
     in
       EmbeddedScheduler.add_request
       (Www.make link)
       (embed_ctx#base)
       (* the continuation: it will receive the document *)
       (fun _url doc ->
     let doc = {
       document_address = doc.document_address;
       document_data = doc.document_data;
       document_headers = Http_headers.merge_headers doc.document_headers
                        ["Content-Type: " ^ given_type]
       }  in
         (* Destroy the alt window *)
     List.iter Tk.destroy (Winfo.children frame);
     (* Add to our table/notify *)
     add_embed emb;
     viewer parms frame embed_ctx doc)
       (Tk_progress.meter frame)
   with
     Not_found -> (* no viewer for this *)
      let t = s_ "Embed Error: no viewer for type %s" given_type in
      pack[Label.create frame [Text t]][]
   | Invalid_request (w,msg) ->
       let t = s_ "Embed Error: %s\n(%s)" (Url.string_of w.www_url) msg in
       pack [Message.create frame [Text t]][]
   | Invalid_link _err ->
       let t = s_ "Embed Error: invalid link" in
       pack [Message.create frame [Text t ]][]
  with
     Not_found -> (* not type given, we have to retrieve to know *)
       (* Firing the request *)
       try
     EmbeddedScheduler.add_request
     (Www.make link)
     (embed_ctx#base)
     (* the continuation: it will receive the document *)
     (* In general, we don't know the type before we get the document *)
     (fun _url doc -> embedded_viewer frame embed_ctx doc)
     (Tk_progress.meter frame)
       with
     Invalid_request (w,msg) ->
       let t = s_ "Embed Error: %s\n(%s)" (Url.string_of w.www_url) msg in
       pack [Message.create frame [Text t]][]
       | Invalid_link _err ->
       let t = s_ "Embed Error: invalid link" in
       pack [Message.create frame [Text t ]][]
(*e: function [[Embed.add]] *)


(*s: function [[Embed.update]] *)
let update frame embed_ctx doc notchanged =
  try
    (* find the date of previous download, (or last-modified ?) *)
    let date_received = get_header "date" doc.document_headers in
    let rewrite_wr wr =
      wr.www_headers <- 
     ("If-Modified-Since: "^date_received) :: wr.www_headers;
      wr.www_headers <- "Pragma: no-cache" :: wr.www_headers;
      wr
    in
    let link = Hyper.default_link (Url.string_of doc.document_address) in
    (* wrapped viewer : decide if we need to redisplay or not *)
    let smart_viewer stdviewer frame embed_ctx newdoc =
      let newdate = 
    try get_header "date"  newdoc.document_headers with Not_found -> "foo"
      in if newdate <> date_received then begin
    List.iter Tk.destroy (Winfo.children frame);
    stdviewer frame embed_ctx newdoc
      end else notchanged()
    in
  (* Check if the type is defined and a viewer available *)
    try
      let given_type = List.assoc "type" embed_ctx#params in
      let ((typ,subtyp), parms) = Lexheaders.media_type given_type in
      try
      let viewer = 
      try Hashtbl.find embedded_viewers (typ,subtyp)
      with Not_found -> Hashtbl.find embedded_viewers (typ, "*")
      in
       EmbeddedScheduler.add_request
       (rewrite_wr (Www.make link))
       (embed_ctx#base)
       (* the continuation: it will receive the document *)
       (fun _url doc ->
     let doc = {
       document_address = doc.document_address;
       document_data = doc.document_data;
       document_headers = Http_headers.merge_headers doc.document_headers
                        ["Content-Type: " ^ given_type]
       }  in
     smart_viewer (viewer parms) frame embed_ctx doc)
       (Tk_progress.meter frame)
      with
      Not_found -> (* no viewer for this *)
      let t = s_ "Embed Error: no viewer for type %s" given_type in
      pack[Label.create frame [Text t]][]
      | Invalid_request (w,msg) ->
      let t = s_ "Embed Error: %s\n(%s)" (Url.string_of w.www_url) msg in
      pack [Message.create frame [Text t]][]
      | Invalid_link _err ->
      let t = s_ "Embed Error: invalid link" in
      pack [Message.create frame [Text t ]][]
    with
      Not_found -> (* not type given, we have to retrieve to know *)
       (* Firing the request *)
        try
      EmbeddedScheduler.add_request
        (rewrite_wr (Www.make link))
        (embed_ctx#base)
     (* the continuation: it will receive the document *)
     (* In general, we don't know the type before we get the document *)
        (fun _url doc -> smart_viewer embedded_viewer frame embed_ctx doc)
        (Tk_progress.meter frame)
        with
      Invalid_request (w,msg) ->
        let t = s_ "Embed Error: %s\n(%s)" (Url.string_of w.www_url) msg in
        pack [Message.create frame [Text t]][]
        | Invalid_link _err ->
        let t = s_ "Embed Error: invalid link" in
        pack [Message.create frame [Text t ]][]
  with
    Not_found -> (* Document has no Date: header *)
      notchanged() 
(*e: function [[Embed.update]] *)
(*e: viewers/embed.ml *)

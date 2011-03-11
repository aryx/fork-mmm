open Printf
open Unix
open Tk
open Hyper
open Www
open Url
open Uri
open Document
open Http_headers
open Viewers
open Embed

(* Navigation *)

type t = {
  nav_id : int;  (* key for the gfx cache *)
  nav_viewer_frame : Widget.widget;
  nav_error : Error.t;			(* popping error dialogs *)
  nav_add_hist : document_id -> string option -> unit;
  nav_show_current: display_info -> string option -> unit;
  nav_log : string -> unit;
  nav_new : Hyper.link -> unit;
  nav_add_active : Url.t -> (unit -> unit) -> unit;
  nav_rem_active : Url.t -> unit
 }

exception Duplicate of Url.t

(* Important note: we assume two requests on the same url are identical
   (when we control emission of requests). This is not the case for 
   POST requests, because we would need to check the POST data.
   This means that you can't post twice *simultaneously* on the same
   url. Proper fix: change the equality semantics of active cnx
 *)

(* Some requests should not be looked for in the cache *)
let dont_check_cache wwwr =
  (match wwwr.www_link.h_method with
      POST _ -> true
    | _ -> false)

(* [request nav usecache wrapwr process specific] produces a function that
   takes an hyperlink, and apply the given behavior to it.
   [usecache] : do we look in the cache to see if we have it already
   [process nav wr dh] : what to to with the retrieved document
   [specific nav did wr] : some specific behavior, checked before we
     look in the cache. Must either raise Not_found or process completely
     the link
   [wrapwr wr] : returns a modified wr
 *)

let request nav usecache wrapwr process specific =
  (* Normally execute the request and process its answer (dh) *)
  let rec retrieve_and_handle wr =
    match Retrieve.f wr handle_link
	{ document_finish = (fun _ -> nav.nav_rem_active wr.www_url);
	  document_process = (fun dh ->
	    process nav wr dh;
            nav.nav_rem_active wr.www_url)}
    with
    | Retrieve.Started abort -> nav.nav_add_active wr.www_url abort
    | Retrieve.InUse -> raise (Duplicate wr.www_url)
	  
  (* Wrapper to deal with general/specific cache *)
  and handle_wr wr =
    try
      match wr.www_url.protocol with
      	MAILTO -> Mailto.f wr
	(* mailto: is really a pain. It doesn't fit the retrieval semantics
	   of WWW requests. *)
      | _ ->
      	  if (not usecache) || dont_check_cache wr then retrieve_and_handle wr
	  else
          (* If the the document can be cached, then it is with no_stamp *)
	    let did = {document_url = wr.www_url; document_stamp = no_stamp} in
	    try
	      specific nav did wr
	    with
	      Not_found ->
              	try
	     	  let doc = Cache.find did in
	     	  try (* display it from source *)
	            process nav wr (Cache.make_handle wr doc)
	     	  with
		    Sys_error s ->
		      wr.www_error#f 
			(I18n.sprintf
			   "Cache error occurred during save of temporary buffer (%s)"
			   s)
	     	  | Unix_error (e,fname,arg) ->
		      wr.www_error#f
			(I18n.sprintf 
			   "Cache error occurred when opening temporary file\n%s: %s (%s)"
			   fname (Unix.error_message e) arg)
              	with 
	     	  Not_found -> (* we don't have the document *)
		    retrieve_and_handle wr
    with
      Duplicate url ->
       	wr.www_error#f (I18n.sprintf "The document %s\nis currently being retrieved for some other purpose.\nMMM cannot process your request until retrieval is completed." (Url.string_of url))

  and handle_link h =
    try (* Convert the link into a request *)
      let wr = Plink.make h in
      wr.www_error <- nav.nav_error;
      handle_wr (wrapwr wr)
    with
      Invalid_link msg ->
      	nav.nav_error#f (I18n.sprintf "Invalid link")
    | Invalid_request (wr, msg) ->
      	nav.nav_error#f (I18n.sprintf "Invalid request %s\n%s"
		           (Url.string_of wr.www_url) msg)
  in
  handle_link

(*
 * Three instances of this general mechanism : view, save, head
 *)
let nothing_specific nav did wr = raise Not_found

(* Specific handling of "view" requests *)
let process_viewer addhist make_ctx = fun nav wr dh ->
  let ctx = make_ctx nav dh.document_id in
  match Viewers.view nav.nav_viewer_frame ctx dh with
    None -> () (* external viewer *)
  | Some di ->
      Gcache.add nav.nav_id dh.document_id di;
      if addhist then nav.nav_add_hist dh.document_id dh.document_fragment;
      nav.nav_show_current di dh.document_fragment

(* check the widget cache *)
let specific_viewer addhist = fun nav did wr ->
  let di = Gcache.find nav.nav_id did in
  if addhist then nav.nav_add_hist did wr.www_fragment;
  (* make it our current displayed document, since it is available *)
  nav.nav_show_current di wr.www_fragment


(* Specific handling of "save" requests *)
let process_save dest = fun nav wr dh ->
  match dh.document_status with
    200 -> Save.transfer wr dh dest
  | n ->
    if wr.www_error#choose 
      	 (I18n.sprintf "Request for %s\nreturned %d %s.\nDo you wish to save ?"
	     (Url.string_of wr.www_url) n (status_msg dh.document_headers))
    then Save.transfer wr dh dest
    else dclose true dh

(* Simple implementation of HEAD *)

let display_headers dh =
  let mytop = Toplevel.create Widget.default_toplevel [] in
    Wm.title_set mytop 
       (sprintf "HEAD %s" (Url.string_of dh.document_id.document_url));
    let hs =
      List.map (function h -> Label.create mytop [Text h; Anchor W])
               dh.document_headers in
     pack (List.rev hs) [Fill Fill_X];
  let b = Button.create mytop
             [Command (fun _ -> destroy mytop); Text "Dismiss"] in
     pack [b] [Anchor Center]
 
let process_head = fun nav wr dh ->
  dclose true dh;
  display_headers dh

(* But for head, we need to change the hlink *)
let make_head hlink =
  { h_uri = hlink.h_uri;
    h_context = hlink.h_context;
    h_method = HEAD;
    h_params = hlink.h_params
    }

(*
 *  Other handlers, less general
 *)

(* Copying a link to the X Selection *)
let copy_link nav h =
  try Frx_selection.set (Hyper.string_of h)
  with Invalid_link msg ->
    nav.nav_error#f (I18n.sprintf "Invalid link")

let user_navigation = ref []
let add_user_navigation (s : string) (f : Viewers.hyper_func) =
  user_navigation := (s,f) :: !user_navigation

let id_wr wr = wr

(* WARNING: we take copies of these objects, so "self" must *not* be
 * captured in a closure (it would always point to the old object).
 * A new object is created for each new top viewer (follow_link).
 * AND for each frame_goto operation.
 *)
class stdctx (did, nav) =
 object (self)
  inherit Viewers.context (did, []) as super
  (* val nav = nav *)  
  (* val did = did *)

  method log = nav.nav_log
  method init =
    (* a new context for a toplevel window *)
    let make_ctx nav did = 
      ((new stdctx(did, nav))#init :> Viewers.context) in
    (* a new context for an embedded window *)
    let make_embed_ctx w targets = 
      let targets = 
	("_self", w) :: ("_parent", Winfo.parent w) :: (frame_fugue targets) in
      let newctx = (new stdctx(did,nav))#init in
      begin
	try 
	  let f = List.assoc "pointsto" self#hyper_funs in
	  let g = List.assoc "clearpointsto" self#hyper_funs in
	  newctx#add_nav ("pointsto", f);
	  newctx#add_nav ("clearpointsto", g);
	with
	  Not_found -> ()
      end;
      (newctx#for_embed [] targets :> Viewers.context) in
    (* by default, use the cache, don't touch the request *)
    let default_request = request nav true id_wr in
    let follow_link _ = 
      default_request (process_viewer true make_ctx) (specific_viewer true)
    and save_link _ =
      default_request (process_save None) nothing_specific
    and copy_link _ = copy_link nav
    and head_link = 
      let f = default_request process_head nothing_specific in
      (fun _ hlink -> f (make_head hlink))
    and new_link _ = nav.nav_new
    in 
    let frame_goto targets hlink =
      try
      (* target semantics PR-HTML 4.0 16.3.2 *)
      	match List.assoc "target" hlink.h_params with
      	| "_blank" ->
	    let w = Toplevel.create Widget.default_toplevel [] in
	    Embed.add { 
	    embed_hlink = hlink;
	    embed_frame = w;
	    embed_context = make_embed_ctx w targets;
	    embed_map = Maps.NoMap;
	    embed_alt = "" }
      	| "_self" ->
	    let w = List.assoc "_self" targets in
	    Embed.add {
	    embed_hlink = hlink;
	    embed_frame = w;
	    embed_context = make_embed_ctx w targets;
	    embed_map = Maps.NoMap;
	    embed_alt = "" }
      	| "_top" -> follow_link targets hlink
      	| "_parent" ->
	    let w = List.assoc "_parent" targets in
	    Embed.add { 
	    embed_hlink = hlink;
	    embed_frame = w;
	    embed_context = make_embed_ctx w targets;
	    embed_map = Maps.NoMap;
	    embed_alt = "" }
      	| s ->
	    let w = List.assoc s targets in
	    Embed.add {
	    embed_hlink = hlink;
	    embed_frame = w;
	    embed_context = make_embed_ctx w targets;
	    embed_map = Maps.NoMap;
	    embed_alt = "" }
      with
      	Not_found -> (* if we are in a frame, it is available as _self *)
	  try
	    let w = List.assoc "_self" targets in
	    Embed.add {
	    embed_hlink = hlink;
	    embed_frame = w;
	    embed_context = make_embed_ctx w targets;
	    embed_map = Maps.NoMap;
	    embed_alt = "" }
	  with
	    Not_found -> follow_link targets hlink
    in
    List.iter super#add_nav !user_navigation;
    List.iter (fun (name, f, txt) ->
      self#add_nav
	(name, {hyper_visible = true; hyper_func = f; hyper_title = txt}))
       ["copy", copy_link, I18n.sprintf "Copy this Link to clipboard";
	"head", head_link, I18n.sprintf "Headers of document";
	"save", save_link, I18n.sprintf "Save this Link";
	"gotonew", new_link, I18n.sprintf "New window with this Link";
        "goto", frame_goto, I18n.sprintf "Open this Link";
       ];
    self
end

let make_ctx nav did = 
  let o = new stdctx(did, nav) in
  ignore (o#init);
  (o :> Viewers.context)

(* Simple wrappers *)
let save_link nav whereto =
  request nav true id_wr (process_save whereto) nothing_specific
let follow_link nav =
  request nav true id_wr (process_viewer true make_ctx) (specific_viewer true)
	
(*
 * Other navigation functions
 *)

(* Used outside an hyperlink *)
let absolutegoto nav uri =
  let follow_link = 
    request nav true id_wr 
      (process_viewer true make_ctx) (specific_viewer true)
  in
  follow_link { h_uri = uri; h_context = None; h_method = GET; h_params = []}
    
(* Used by navigators for back/forward/reload *)
let historygoto nav did frag usecache =
  Log.debug "historygoto";
  if did.document_stamp = no_stamp then begin
    (* we can safely consider this as normal navigation *)
    let uri = match frag with
	       None -> Url.string_of did.document_url
	     | Some f ->
	        sprintf "%s#%s" (Url.string_of did.document_url) f 
    in
    (* modify wr *)
    let follow_link =
      request nav usecache
	(function wr ->
	  if not usecache then
	    wr.www_headers <- "Pragma: no-cache" :: wr.www_headers;
	  wr)
	(process_viewer false make_ctx) (* don't add to history *)
	(specific_viewer false)
    in
    follow_link { h_uri = uri;
		  h_context = None;
		  h_method = GET;
		  h_params = []};
    true
  end else begin
    (* the url is a "non-unique" document, that is, its url is not
       enough to retrieve the document. We should not attempt to
       reload or retrieve if flushed from the cache
    *)
    try
      let di = Gcache.find nav.nav_id did in
	 nav.nav_show_current di frag;
	 true
    with
      Not_found ->
        nav.nav_error#f 
	 (I18n.sprintf "Document was flushed from cache, and should be reloaded from its url\n(probably a POST request)");
        false
   end


let update nav did nocache =
  (* This gets called if answer is 200 but also 304 *)
  let process_update nav wr dh =
    match dh.document_status with
      304 -> 
	Cache.patch dh.document_id dh.document_headers;
	dclose true dh;
	begin try
	  let di = Gcache.find nav.nav_id did in
	  di#di_update
	with
	  Not_found -> () (* weird *)
	end;
	wr.www_error#ok (I18n.sprintf "Document %s has not changed.\n"
			   (Url.string_of wr.www_url))
    | 200 | _ ->
        (* kill the previous displayed window *)
    	Gcache.displace nav.nav_id did;
	(* we may have been redirected : check new did *)
	let oldurl = Url.string_of did.document_url in
	let newurl = Url.string_of dh.document_id.document_url in
	let add_hist = oldurl <> newurl in
	if add_hist then 
	  wr.www_error#ok (I18n.sprintf "Document %s is relocated to:\n%s"
			     oldurl newurl);
    	wr.www_logging <- nav.nav_log;
    	process_viewer add_hist make_ctx nav wr dh
  in
  try
    let doc = Cache.find did in
    try
      (* find the date of previous download, (or last-modified ?) *)
      let date_received = get_header "date" doc.document_info in
      let follow_link =
	request nav 
	  false (* we don't want to use cache here *)
          (* setup additional headers *)
	  (fun wr -> 
	    wr.www_headers <- 
	       ("If-Modified-Since: "^date_received) :: wr.www_headers;
	    if nocache then
	      wr.www_headers <- "Pragma: no-cache" :: wr.www_headers;
	    wr)
	  process_update nothing_specific in
      follow_link { h_uri = Url.string_of did.document_url;
		    h_context = None;
		    h_method = GET;
		    h_params = []}
    with
      Not_found ->
	nav.nav_error#f ("Document has no Date: header.")
  with
    Not_found ->
      nav.nav_error#f (I18n.sprintf "Document %s\nhas been flushed from cache"
			            (Url.string_of did.document_url))
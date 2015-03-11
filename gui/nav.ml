(*s: ./gui/nav.ml *)
open I18n
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

(*s: type Nav.t *)
type t = {
  nav_viewer_frame : Widget.widget;

  (* Nav.absolutegoto -> request -> process_viewer -> <> *)
  nav_show_current: Viewers.display_info -> string option -> unit;

  (*s: [[Nav.t]] manage history methods *)
  nav_add_hist : Document.document_id -> string option -> unit;
  (*e: [[Nav.t]] manage history methods *)
  (*s: [[Nav.t]] manage active connections methods *)
  nav_add_active : Url.t -> Www.aborter -> unit;
  nav_rem_active : Url.t -> unit;
  (*e: [[Nav.t]] manage active connections methods *)

  (*s: [[Nav.t]] graphic cache related methods *)
  nav_id : int;  (* key for the gfx cache *)
  (*e: [[Nav.t]] graphic cache related methods *)
  (*s: [[Nav.t]] error methods *)
  nav_error : Error.t;			(* popping error dialogs *)
  (*e: [[Nav.t]] error methods *)
  (*s: [[Nav.t]] logging method *)
  nav_log : string -> unit;
  (*e: [[Nav.t]] logging method *)

  (*s: [[Nav.t]] other fields *)
  nav_new : Hyper.link -> unit;
  (*e: [[Nav.t]] other fields *)
 }
(*e: type Nav.t *)

(*s: exception Nav.Duplicate *)
exception Duplicate of Url.t
(*e: exception Nav.Duplicate *)

(* Important note: we assume two requests on the same url are identical
   (when we control emission of requests). This is not the case for 
   POST requests, because we would need to check the POST data.
   This means that you can't post twice *simultaneously* on the same
   url. Proper fix: change the equality semantics of active cnx
 *)

(*s: function Nav.dont_check_cache *)
(* Some requests should not be looked for in the cache *)
let dont_check_cache wwwr =
  match wwwr.www_link.h_method with
  | POST _ -> true
  | _ -> false
(*e: function Nav.dont_check_cache *)

(*s: function Nav.request *)
(* [request nav usecache wrapwr process specific] produces a function that
   takes an hyperlink, and apply the given behavior to it.
   [usecache] : do we look in the cache to see if we have it already
   [process nav wr dh] : what to to with the retrieved document
   [specific nav did wr] : some specific behavior, checked before we
     look in the cache. Must either raise Not_found or process completely
     the link
   [wrapwr wr] : returns a modified wr
 *)
let request nav process (usecache, wrapwr, specific) lk =

  (*s: function Nav.request.retrieve_and_handle *)
  (* Normally execute the request and process its answer (dh) *)
  let rec retrieve_and_handle wr =
    let cont = 
      { document_process = (fun dh ->
          process nav wr dh;
          nav.nav_rem_active wr.www_url
        );
        document_finish = (fun _ -> 
          nav.nav_rem_active wr.www_url
        );
      }
    in
    match Retrieve.f wr handle_link cont with
    | Retrieve.Started aborter -> 
        nav.nav_add_active wr.www_url aborter
    | Retrieve.InUse -> 
        raise (Duplicate wr.www_url)
  (*e: function Nav.request.retrieve_and_handle *)
  (*s: function Nav.request.handle_wr *)
  (* Wrapper to deal with general/specific cache *)
  and handle_wr wr =
    try
      match wr.www_url.protocol with
      (*s: [[Nav.request.handle_wr()]] match protocol special cases *)
      | MAILTO -> Mailto.f wr
       (* mailto: is really a pain. It doesn't fit the retrieval semantics
        * of WWW requests. *)
      (*e: [[Nav.request.handle_wr()]] match protocol special cases *)
      | _ ->
         if (not usecache) || dont_check_cache wr 
         then retrieve_and_handle wr
         else
           (*s: [[Nav.request.handle_wr()]] if use cache *)
           (* If the the document can be cached, then it is with no_stamp *)
           let did = { document_url = wr.www_url; document_stamp = no_stamp } in
           try
             specific nav did wr
           with Not_found ->
             try
               let doc = Cache.find did in
               try (* display it from source *)
                 process nav wr (Cache.make_handle wr doc)
               with 
               | Sys_error s ->
                   wr.www_error#f (s_
                    "Cache error occurred during save of temporary buffer (%s)"
                       s)
               | Unix_error (e,fname,arg) ->
                   wr.www_error#f 
                     (s_ "Cache error occurred when opening temporary file\n%s: %s (%s)"
                            fname (Unix.error_message e) arg)
            with Not_found -> (* we don't have the document *)
              retrieve_and_handle wr
           (*e: [[Nav.request.handle_wr()]] if use cache *)
    with Duplicate url ->
      wr.www_error#f (s_ "The document %s\nis currently being retrieved for some other purpose.\nMMM cannot process your request until retrieval is completed." (Url.string_of url))
  (*e: function Nav.request.handle_wr *)
  (*s: function Nav.request.handle_link *)
  and handle_link h =
    try (* Convert the link into a request *)
      let wr = Plink.make h in
      wr.www_error <- nav.nav_error;
      wr |> wrapwr |> handle_wr
    with
    | Hyper.Invalid_link msg ->
        nav.nav_error#f (s_ "Invalid link")
    | Www.Invalid_request (wr, msg) ->
        nav.nav_error#f (s_ "Invalid request %s\n%s"(Url.string_of wr.www_url)msg)
  in
  (*e: function Nav.request.handle_link *)
  handle_link lk
(*e: function Nav.request *)

(*s: function Nav.nothing_specific *)
(*
 * Three instances of this general mechanism : view, save, head
 *)
let nothing_specific nav did wr = raise Not_found
(*e: function Nav.nothing_specific *)

(*s: function Nav.process_viewer *)
(* Specific handling of "view" requests *)
let process_viewer addhist make_ctx = 
 fun nav _wr dh ->
  let ctx = make_ctx nav dh.document_id in
  match Viewers.view nav.nav_viewer_frame ctx dh with
  | None -> () (* external viewer *)
  | Some di ->
      (*s: [[Nav.process_viewer()]] add in cache and history the document *)
      if addhist 
      then nav.nav_add_hist dh.document_id dh.document_fragment;
      (*x: [[Nav.process_viewer()]] add in cache and history the document *)
      Gcache.add nav.nav_id dh.document_id di;
      (*e: [[Nav.process_viewer()]] add in cache and history the document *)
      nav.nav_show_current di dh.document_fragment
(*e: function Nav.process_viewer *)

(*s: function Nav.specific_viewer *)
(* check the widget cache *)
let specific_viewer addhist = fun nav did wr ->
  let di = Gcache.find nav.nav_id did in
  if addhist then nav.nav_add_hist did wr.www_fragment;
  (* make it our current displayed document, since it is available *)
  nav.nav_show_current di wr.www_fragment
(*e: function Nav.specific_viewer *)


(*s: function Nav.process_save *)
(* Specific handling of "save" requests *)
let process_save dest = fun nav wr dh ->
  match dh.document_status with
    200 -> Save.transfer wr dh dest
  | n ->
    if wr.www_error#choose 
        (s_ "Request for %s\nreturned %d %s.\nDo you wish to save ?"
              (Url.string_of wr.www_url) n (status_msg dh.dh_headers))
    then Save.transfer wr dh dest
    else dclose true dh
(*e: function Nav.process_save *)

(*s: function Nav.display_headers *)
(* Simple implementation of HEAD *)

let display_headers dh =
  let mytop = Toplevel.create Widget.default_toplevel [] in
  Wm.title_set mytop 
      (sprintf "HEAD %s" (Url.string_of dh.document_id.document_url));
  let hs =
    dh.dh_headers |> List.map (fun h -> Label.create mytop [Text h; Anchor W])
  in
  pack (List.rev hs) [Fill Fill_X];
  let b = Button.create mytop
            [Command (fun _ -> destroy mytop); Text "Dismiss"] in
  pack [b] [Anchor Center]
(*e: function Nav.display_headers *)
 
(*s: constant Nav.process_head *)
let process_head = fun nav wr dh ->
  dclose true dh;
  display_headers dh
(*e: constant Nav.process_head *)

(*s: function Nav.make_head *)
(* But for head, we need to change the hlink *)
let make_head hlink =
  { hlink with h_method = HEAD; }
(*e: function Nav.make_head *)

(*
 *  Other handlers, less general
 *)

(*s: function Nav.copy_link *)
(* Copying a link to the X Selection *)
let copy_link nav h =
  try 
    Frx_selection.set (Hyper.string_of h)
  with Invalid_link msg ->
    nav.nav_error#f (s_ "Invalid link")
(*e: function Nav.copy_link *)

(*s: constant Nav.user_navigation *)
let user_navigation = ref []
(*e: constant Nav.user_navigation *)
(*s: function Nav.add_user_navigation *)
let add_user_navigation (s : string) (f : Viewers.hyper_func) =
  user_navigation := (s,f) :: !user_navigation
(*e: function Nav.add_user_navigation *)

(*s: function Nav.id_wr *)
let id_wr wr = wr
(*e: function Nav.id_wr *)

(*s: class Nav.stdctx *)
(* WARNING: we take copies of these objects, so "self" must *not* be
 * captured in a closure (it would always point to the old object).
 * A new object is created for each new top viewer (follow_link).
 * AND for each frame_goto operation.
 *)
class stdctx (did, nav) =
 object (self)
  inherit Viewers.context (did, []) as super
  (* val did = did *)
  (* val nav = nav *)  

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
        with Not_found -> ()
      end;
      (newctx#for_embed [] targets :> Viewers.context) in

    (* by default, use the cache, don't touch the request *)
    let follow_link _ = 
      request nav (process_viewer true make_ctx) 
        (true, id_wr, specific_viewer true)
    and save_link _ =
      request nav (process_save None) (true, id_wr, nothing_specific)
    and copy_link _ = 
      copy_link nav
    and head_link = 
      let f = request nav process_head (true, id_wr, nothing_specific) in
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
    !user_navigation |> List.iter super#add_nav;

    ["copy", copy_link, s_ "Copy this Link to clipboard";
     "head", head_link, s_ "Headers of document";
     "save", save_link, s_ "Save this Link";
     "gotonew", new_link, s_ "New window with this Link";
     "goto", frame_goto, s_ "Open this Link";
    ] |> List.iter (fun (name, f, txt) ->
        self#add_nav (name, { hyper_visible = true; 
                              hyper_func = f; 
                              hyper_title = txt })
    );
    self

end
(*e: class Nav.stdctx *)

(*s: function Nav.make_ctx *)
let make_ctx nav did = 
  ((new stdctx(did, nav))#init :> Viewers.context)
(*e: function Nav.make_ctx *)

(*s: function Nav.save_link *)
(* Simple wrappers *)
let save_link nav whereto =
  request nav (process_save whereto) (true, id_wr, nothing_specific)
(*e: function Nav.save_link *)
(*s: function Nav.follow_link *)
let follow_link nav lk =
  lk |> request nav (fun nav wr dh -> process_viewer true make_ctx nav wr dh)
    (*s: [[Nav.follow_link]] extra arguments to Nav.request *)
    (true, id_wr, specific_viewer true)
    (*e: [[Nav.follow_link]] extra arguments to Nav.request *)
(*e: function Nav.follow_link *)
    
(*
 * Other navigation functions
 *)

(*s: function Nav.absolutegoto *)
(* Used outside an hyperlink *)
let absolutegoto nav uri =
  follow_link nav (Hyper.default_link uri)
(*e: function Nav.absolutegoto *)
    
(*s: function Nav.historygoto *)
(* Used by navigators for back/forward/reload *)
let historygoto nav did frag usecache =
  Log.debug "historygoto";
  if did.document_stamp = no_stamp then begin
    (* we can safely consider this as normal navigation *)
    let uri = 
      match frag with
      | None -> Url.string_of did.document_url
      | Some f -> sprintf "%s#%s" (Url.string_of did.document_url) f 
    in
    (* modify wr *)
    let follow_link lk =
      lk |> request nav  
        (process_viewer false make_ctx) (* don't add to history *)
        (usecache,
         (fun wr ->
            if not usecache 
            then wr.www_headers <- "Pragma: no-cache" :: wr.www_headers;
            wr),
         specific_viewer false)
    in
    follow_link (Hyper.default_link uri);
    true
  end else begin
    (* the url is a "non-unique" document, that is, its url is not
     * enough to retrieve the document. We should not attempt to
     * reload or retrieve if flushed from the cache
     *)
    try
      let di = Gcache.find nav.nav_id did in
      nav.nav_show_current di frag;
      true
    with Not_found ->
      nav.nav_error#f (s_ "Document was flushed from cache, and should be reloaded from its url\n(probably a POST request)");
      false
   end
(*e: function Nav.historygoto *)


(*s: function Nav.update *)
let update nav did nocache =
  (* This gets called if answer is 200 but also 304 *)
  let process_update nav wr dh =
    match dh.document_status with
      304 -> 
    Cache.patch dh.document_id dh.dh_headers;
    dclose true dh;
    begin try
      let di = Gcache.find nav.nav_id did in
      di#di_update
    with
      Not_found -> () (* weird *)
    end;
    wr.www_error#ok 
      (s_ "Document %s has not changed.\n" (Url.string_of wr.www_url))
    | 200 | _ ->
        (* kill the previous displayed window *)
     Gcache.displace nav.nav_id did;
    (* we may have been redirected : check new did *)
    let oldurl = Url.string_of did.document_url in
    let newurl = Url.string_of dh.document_id.document_url in
    let add_hist = oldurl <> newurl in
    if add_hist then 
      wr.www_error#ok (s_ "Document %s is relocated to:\n%s" oldurl newurl);
     wr.www_logging <- nav.nav_log;
     process_viewer add_hist make_ctx nav wr dh
  in
  try
    let doc = Cache.find did in
    try
      (* find the date of previous download, (or last-modified ?) *)
      let date_received = get_header "date" doc.document_headers in
      let follow_link =
        request nav process_update
        (false, (* we don't want to use cache here *)
         (* setup additional headers *)
         (fun wr -> 
           wr.www_headers <- 
             ("If-Modified-Since: "^date_received) :: wr.www_headers;
           if nocache 
           then wr.www_headers <- "Pragma: no-cache" :: wr.www_headers;
           wr),
         nothing_specific)
      in
      follow_link (Hyper.default_link (Url.string_of did.document_url))
    with Not_found ->
      nav.nav_error#f ("Document has no Date: header.")
  with Not_found ->
   nav.nav_error#f (s_ "Document %s\nhas been flushed from cache"
                        (Url.string_of did.document_url))
(*e: function Nav.update *)
(*e: ./gui/nav.ml *)

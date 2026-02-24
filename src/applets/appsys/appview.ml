(*s: appview.ml *)

(* was in viewers.mli
class trivial_display : (Widget.widget * Url.t) -> (* #display_info *)
(* boilerplate class type *)
object
  method di_abort : unit
  method di_destroy : unit
  method di_fragment : string option -> unit
  method di_last_used : int
  method di_load_images : unit
  method di_redisplay : unit
  method di_source : unit
  method di_title : string
  method di_touch : unit
  method di_widget : Widget.widget
  method di_update : unit
end
*)

class trivial_display (w, url) =
 object
  inherit Viewers.display_info ()
  (* val w = w *)
  (* val url = url *)

  method di_widget = w
  method di_abort = ()
  method di_destroy = if Winfo.exists w then Tk.destroy w
  method di_fragment _f = ()
  method di_redisplay = ()
  method di_title = Url.string_of url
  method di_source = ()
  method di_load_images = ()
  method di_update = ()
end



(* Wrapping up
 * When EMBED is recognized by the HTML display machine, the "embedded object"
 * is passed to the embed manager/scheduler with some viewer as continuation.
 * This module defines this viewer, which takes as arguments
 *   [parms] MIME parameters
 *   [frame] embedded frame in HTML widget
 *   [ctx] Viewers.context
 *   [doc] : definition of document
 * When the viewer is called, the bytecode may or may not have been loaded
 * (the embed manager only stores the applet in a file, it doesn't load it;
 *  the first invocation of the applet has the responsability to actually
 *  load the file)
 *)

(*s: function [[Appview.is_update]] *)
(* Note: since we look in the cache of loaded applets, we must check 
 * that this is actually the *same* version of the applet that we
 * are trying to run
 *)
let is_update this_headers loaded_headers =
  try
    let this_date = Http_headers.get_header "date" this_headers
    and loaded_date = Http_headers.get_header "date" loaded_headers in
     this_date <> loaded_date
  with
    Not_found ->  (* no date means update *)
      true
(*e: function [[Appview.is_update]] *)

(*s: function [[Appview.applet_viewer]] *)
let applet_viewer _parms frame ctx (doc : Document.t) =
  let url = doc.document_address in
  let invoke frame ftable =
    (* we need another level of frame where to bind the update event *)
    let f = Frame.create frame [Class "Caml"] in
    Tk.pack [f][Expand true; Fill Fill_Both];
    (* if we are asked to update, do it *)
    let caps = Cap.network_caps_UNSAFE () in
    Frx_synth.bind f "update"
      (fun _ -> Embed.update caps frame ctx doc (fun () -> ()));
    Applets.call ftable f ctx
  in
  try
    (* If the bytecode is loaded, run the thing; else fail *)
    match Dload.get url with
    | Rejected old ->
    if is_update doc.document_headers old then begin
      Dload.remove url;
      raise Not_found
    end else
    Applets.error frame (I18n.sprintf "%s was rejected" (Url.string_of url))
    | Unavailable old ->
    if is_update doc.document_headers old then begin
      Dload.remove url;
      raise Not_found
    end else
      Applets.error frame (I18n.sprintf "%s is not available" (Url.string_of url));
    | Loaded cmo ->
    if is_update doc.document_headers cmo.module_info then begin
      Dload.remove url;
      raise Not_found
    end else
      invoke frame cmo.module_functions
  with
    Not_found ->
      (* Otherwise, queue it, and if it's the first request for this applet,
     load the bytecode *)
      if Dload.add_pending_applet url (invoke frame)
      then
     Dload.load doc (* This will flush the queue of invocations.
(*e: function [[Appview.applet_viewer]] *)
              Since loading is interactive, other calls 
              to applet_viewer for the same applet may occur
              concurrently, in which case their invocation
              will be stored in the queue and flushed after
              loading.
              *)

(*s: function [[Appview.code_viewer]] *)
(* If we load code directly (e.g. by clicking on a link pointing to a
   bytecode file).
   If the bytecode has been alread loaded, don't do anything.
   Else, proceed to load it (no entry point available !).
 *)
let code_viewer _parms frame ctx (dh : Document.handle) =
  let url = dh.document_id.document_url in
  try
    match Dload.get url with
    | Rejected old ->
    if is_update dh.dh_headers old then begin
      Dload.remove url;
      raise Not_found
    end else begin
      Document.dclose true dh;
      Error.f (I18n.sprintf "%s was rejected" (Url.string_of url));
      None
    end
    | Unavailable old ->
    if is_update dh.dh_headers old then begin
      Dload.remove url;
      raise Not_found
    end else begin
      Document.dclose true dh;
      Error.f (I18n.sprintf "%s is not available" (Url.string_of url));
      None
    end
    | Loaded cmo ->
    if is_update dh.dh_headers cmo.module_info then begin
      Dload.remove url;
      raise Not_found
    end else begin
      Document.dclose true dh;
      let f = Frame.create frame [] in
      Tk.pack [f][Expand true; Fill Fill_Both];
      Applets.call cmo.module_functions f ctx;
      Some (new trivial_display (f, url))
    end
  with
    Not_found ->
      let f = Frame.create frame [] in
      (* Otherwise, queue it, and if it's the first request for this applet,
     save the code and loadit when finished *)
      if Dload.add_pending_applet url (fun ftable -> Applets.call ftable f ctx)
      then begin
       let fname = Msys.mktemp "bytc"
       and buffer = Bytes.create 2048 in
       let oc = open_out_bin fname in
       dh.document_feed.feed_schedule
          (fun _ ->
            try
              let n = dh.document_feed.feed_read buffer 0 2048 in
              if n = 0 then begin
               Document.dclose true dh;
               close_out oc;
        let doc = Document.{ document_address = dh.document_id.document_url;
                 document_data = FileData(Fpath.v fname,true);
                 document_headers = dh.dh_headers} in
        Cache.add dh.document_id doc;
        Cache.finished dh.document_id;
           Dload.load doc
              end else
        output oc buffer 0 n
            with
              Unix.Unix_error(_,_,_) ->
               Document.dclose true dh;
           close_out oc;
           Msys.rm fname;
           Error.f (I18n.sprintf "Error during loading of bytecode")
          );
      end;
      Some (new trivial_display(f, url))
(*e: function [[Appview.code_viewer]] *)


(*e: appview.ml *)

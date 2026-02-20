(*s: viewers/viewers.ml *)
open Common
open I18n

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Multimedia
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type [[Viewers.vparams]] *)
(* list of additionnal parameters for the viewer, according to its
   activation point *)
(* hyper functions are: "goto", "save", "gotonew" *)
type vparams = (string * string) list
(*e: type [[Viewers.vparams]] *)
(*s: type [[Viewers.frame_targets]] *)
type frame_targets = (string * Widget.widget) list
(*e: type [[Viewers.frame_targets]] *)

(*s: function [[Viewers.frame_adopt]] *)
let frame_adopt w targets = 
  targets |> List.map (function 
    | "_self",_ -> "_self", w
    | "_parent", _ -> "_parent", Winfo.parent w
    | s, f -> s, f
  )
    
(*e: function [[Viewers.frame_adopt]] *)

(*s: function [[Viewers.frame_fugue]] *)
let frame_fugue targets =
  let rec ff accu = function
    | [] -> accu
    | ("_self", _) :: l -> ff accu l
    | ("_parent", _) :: l -> ff accu l
    | p :: l -> ff (p::accu) l
  in
  ff [] targets
(*e: function [[Viewers.frame_fugue]] *)

(*s: type [[Viewers.hyper_func]] *)
type hyper_func = {
  hyper_visible : bool;
  hyper_title : string;

  hyper_func : frame_targets -> Hyper.link -> unit
}
(*e: type [[Viewers.hyper_func]] *)

(*s: class [[Viewers.context]] *)
(* The context given to a viewer *)
class virtual context ((did : Document.id), 
                       (v : vparams)) =
 object (self : 'a)

  val base = did
  method base = base

  val viewer_params = v
  method params = viewer_params

  val mutable (*private*) funs = ([] : (string * hyper_func ) list)
  method hyper_funs = funs

  val targets = []

  method goto hlink    = self#invoke "goto" hlink
  method gotonew hlink = self#invoke "gotonew" hlink
  method save hlink    = self#invoke "save" hlink
  method invoke name hlink =
    try (List.assoc name funs).hyper_func targets hlink
    with Not_found -> ()

  method add_nav (fname, hf) =
    funs <- (fname, hf) :: funs

  (* apply this on a copy ! *)
  method for_embed (vparams: vparams) (newtargets : frame_targets) : 'a =
    {< viewer_params = vparams; 
       targets = 
         let _oldtargets = targets in (* for debug *)
          match newtargets with 
          (* keep exactly the same environment *)
          | [] -> targets 
          (* assume I'm given new _self and _parent *)
          | l -> l @ frame_fugue targets 
    >}
  method in_embed did =
    {< base = did >}

  method virtual log : string -> unit
end
(*e: class [[Viewers.context]] *)

(* The object created/returned by a viewer *)
class  virtual display_info () =
 object (_self : 'a)
  (* boilerplate class decl *)
  (*s: [[Viewers.display_info]] virtual methods signatures *)
  method virtual di_title : string		(* some visible title *)

  (* the created widget containing the graphics *)
  method virtual di_widget : Widget.widget

  (*s: [[Viewers.display_info]] images virtual methods signatures *)
  method virtual di_load_images : unit (* load images *)
  (*e: [[Viewers.display_info]] images virtual methods signatures *)
  (*s: [[Viewers.display_info]] embedded virtual methods signatures *)
  method virtual di_update : unit      (* update embedded objects *)
  (*e: [[Viewers.display_info]] embedded virtual methods signatures *)
  (*s: [[Viewers.display_info]] fragment virtual method signature *)
  method virtual di_fragment : string option -> unit	(* for # URIs *)
  (*e: [[Viewers.display_info]] fragment virtual method signature *)

  (*s: [[Viewers.display_info]] lifecycle virtual methods signatures *)
  method virtual di_abort : unit		 (* stop display *)
  (*x: [[Viewers.display_info]] lifecycle virtual methods signatures *)
  method virtual di_redisplay : unit		(* redisplay *)
  (*e: [[Viewers.display_info]] lifecycle virtual methods signatures *)

  (*s: [[Viewers.display_info]] graphic cache virtual methods signatures *)
  method virtual di_destroy : unit	 (* die *)
  (*e: [[Viewers.display_info]] graphic cache virtual methods signatures *)
  (*s: [[Viewers.display_info]] other virtual methods signatures *)
  method virtual di_source : unit 	        (* source viewer *)
  (*e: [[Viewers.display_info]] other virtual methods signatures *)
  (*e: [[Viewers.display_info]] virtual methods signatures *)
  (*s: [[Viewers.display_info]] graphic cache methods *)
  val mutable di_last_used = !Low.global_time
  method di_last_used = 
    di_last_used
  method di_touch = 
    di_last_used <- !Low.global_time
  (*e: [[Viewers.display_info]] graphic cache methods *)
end

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Viewers.di_compare]] *)
let di_compare di di' = 
  (*di#di_last_used > di'#di_last_used*)
  Stdlib.compare di'#di_last_used di#di_last_used
(*e: function [[Viewers.di_compare]] *)

(*****************************************************************************)
(* External viewer *)
(*****************************************************************************)
(* 
 * The default external viewer
 *)

(*s: function [[Viewers.metamail]] *)
(* Metamail options
   -b : not an RFC822 message
   -z : delete when finished 
   -x : not on a tty 
 *)
let metamail ctype file =
  ignore (Munix.system "metamail -b -z -x -c" [ctype; file] true)
(*e: function [[Viewers.metamail]] *)

(*s: function [[Viewers.extern_batch]] *)
(* Batch version: we transfer everything and then run metamail *)
let _extern_batch dh ctype = 
  let outfile = Msys.mktemp "mmm" in
  Document.add_log dh (
    s_ "Saving %s\nfor external display with MIME type %s"
          (Url.string_of dh.document_id.document_url) ctype)
    (fun () -> Msys.rm outfile);
  let endmsg =
    s_ "Running metamail with MIME media-type: %s" ctype in
    Save.tofile (metamail ctype) (Decoders.insert dh) outfile endmsg
(*e: function [[Viewers.extern_batch]] *)

(*s: function [[Viewers.extern]] *)
(* "interactive" version: 
 *    send data to metamail as it arrives, but allow abort
 * NOTE: There are sometimes weird errors when the child dumps core
 *     	 between fork/exec with no apparent reason (on SunOS4.1 only)
 *)
let extern (dh : Document.handle) (ctype : string) : unit =
  let (pin, pout) = Unix.pipe() in
  (* children must not keep pout open *)
  Unix.set_close_on_exec pout;
  match Low.fork() with
  | 0 ->
      Unix.dup2 pin Unix.stdin; 
      Unix.close pin;
      Munix.execvp "metamail" [| "metamail"; "-b"; "-x"; "-c"; ctype |]
  | pid ->  
      Unix.close pin;
      let kill () = 
         try Unix.kill pid 2
         with Unix.Unix_error (e,_,_) ->
          Log.f (sprintf "Can't kill child (%s)" (Unix.error_message e))
      in
      let url = Url.string_of dh.document_id.document_url in
      Document.add_log dh 
        (s_ "Retrieving %s\nfor external display with MIME type %s" url ctype)
        kill;

      let red = ref 0 in
      let size =   
        try Http_headers.contentlength dh.dh_headers
        with Not_found -> 40000 (* duh *)
      in
      let buffer = Bytes.create 4096 in
      dh.document_feed.feed_schedule
        (fun () ->
          try
            let n = dh.document_feed.feed_read buffer 0 4096 in
            if n = 0 then begin
              Document.dclose true dh;
              Unix.close pout;
              Document.end_log dh (s_ "End of transmission")
            end else begin
              ignore (Unix.write pout buffer 0 n);
              red := !red + n;
              Document.progress_log dh (!red * 100 / size)
            end
          with Unix.Unix_error(e,_,_) ->
            Log.f (sprintf "Error writing to viewer (%s)"
                    (Unix.error_message e));
            Document.dclose true dh;
            kill();
            Unix.close pout;
            Document.destroy_log dh false;
            Error.f (s_ "Error during retrieval of %s" url)
       )
(*e: function [[Viewers.extern]] *)

(*****************************************************************************)
(* Types part 2 *)
(*****************************************************************************)

(*
 * Viewer control
 * Specify on the base of MIME type if we want to
 *   - use an internal displayer (assumed to exist)
 *   - use an external displayer (metamail or other)
 *   - save to file
 *)

(* Table of viewers, according to media-type (MIME)
 * Actually, this is only for internal viewers, since the rest
 * will be passed to metamail.
 *)

(*s: type [[Viewers.t]] *)
(* Definition of an internal viewer *)
type t = 
    Http_headers.media_parameter list -> 
    (Widget.widget ->  context -> Document.handle -> display_info option)
(*e: type [[Viewers.t]] *)

(*s: type [[Viewers.spec]] *)
type spec =
  | Internal of t
  | External
  (*s: [[Viewers.spec]] other cases *)
  (*| Interactive  (* ask what to do about it *)*)
  (*x: [[Viewers.spec]] other cases *)
  | Save	     (* always save *)
  (*e: [[Viewers.spec]] other cases *)
(*e: type [[Viewers.spec]] *)

(*s: constant [[Viewers.viewers]] *)
let viewers : (Http_headers.media_type, spec) Hashtbl.t = 
  Hashtbl_.create ()
(*e: constant [[Viewers.viewers]] *)

(*s: function [[Viewers.add_viewer]] *)
(* That's for internal viewers only *)
let add_viewer ctype viewer =
  Hashtbl.add viewers ctype (Internal viewer)
(*e: function [[Viewers.add_viewer]] *)

(*s: function [[Viewers.rem_viewer]] *)
let rem_viewer ctype =
  Hashtbl.remove viewers ctype
(*e: function [[Viewers.rem_viewer]] *)

(*****************************************************************************)
(* Interactive viewer *)
(*****************************************************************************)

(*s: function [[Viewers.unknown]] *)
let rec unknown (frame : Widget.widget) (ctx : context) (dh : Document.handle)
   : display_info option =
  match Frx_dialog.f frame (Mstring.gensym "error")
         (s_ "MMM Warning")
         (s_ "No MIME type given for the document\n%s"
               (Url.string_of dh.document_id.document_url))
         (Tk.Predefined "question") 
         0
         [s_ "Retry with type"; s_ "Save to file"; s_ "Abort"] 
  with
  | 0 ->
    let v = Textvariable.create_temporary frame in
    Textvariable.set v "text/html";
    if Frx_req.open_simple_synchronous (s_ "MIME type") v then
       let ctype = Textvariable.get v in
       dh.dh_headers <- ("Content-Type: " ^ ctype) :: dh.dh_headers;
       (* try again *)
       f frame ctx dh
    else begin
       Save.interactive (fun _ -> ()) dh;
       None
    end
 | 1 -> Save.interactive (fun _ -> ()) dh; None
 | 2 -> Document.dclose true dh; None
 | _ -> assert false (* property of dialogs *)
(*e: function [[Viewers.unknown]] *)

(*s: function [[Viewers.interactive]] *)
and interactive frame (ctx : context) (dh : Document.handle) (ctype : string) 
   : display_info option =
  match Frx_dialog.f frame (Mstring.gensym "error")
         (s_ "MMM Viewers")
         (s_
          "No behavior specified for MIME type\n%s\ngiven for the document\n%s"
           ctype
           (Url.string_of dh.document_id.document_url))
         (Tk.Predefined "question") 
         0
         [s_ "Retry with another type";
          s_ "Display with metamail";
          s_ "Save to file";
          s_ "Abort"] 
  with
  | 0 ->
      let v = Textvariable.create_temporary frame in
      Textvariable.set v "text/html";
      if Frx_req.open_simple_synchronous (s_ "MIME type") v then
        let ctype = Textvariable.get v 
        in
        dh.dh_headers <- ("Content-Type: " ^ ctype) :: dh.dh_headers;
        (* try again *)
        f frame ctx dh
      else begin
        Save.interactive (fun _ -> ()) dh;
        None
      end
  | 1 -> extern (Decoders.insert dh) ctype; None
  | 2 -> Save.interactive (fun _ -> ()) dh; None
  | 3 -> Document.dclose true dh; None
  | _ -> assert false (* property of dialogs *)
(*e: function [[Viewers.interactive]] *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: function [[Viewers.view]] *)
(* the meat (was called view) *)
(* Nav.absolutegoto -> Nav.request -> Nav.process_viewer (via process) -> <>
 *   -> Plain.viewer | Htmlw.viewer (via viewers) 
 *)
and f frame (ctx : context) (dh : Document.handle) : display_info option =
  try 
    let ctype = Http_headers.contenttype dh.dh_headers in
    let (typ, sub), pars = Lexheaders.media_type ctype in
    try (* Get the viewer *)
      Logs.debug (fun m -> m "Viewers.view %s/%s" typ sub);
      let viewer =
        try Hashtbl.find viewers (typ,sub)
        with Not_found -> 
          Logs.warn (fun m -> m "didn't find viewer for %s/%s" typ sub);
          Hashtbl.find viewers (typ,"*")
      in
      match viewer with
      (*s: [[Viewers.view]] match viewer cases *)
      | Internal viewer ->
          ctx#log (s_ "Displaying...");
          viewer pars frame ctx (Decoders.insert dh)
      (*x: [[Viewers.view]] match viewer cases *)
      | External ->
          ctx#log (s_ "Displaying externally");
          extern (Decoders.insert dh) (sprintf "%s/%s" typ sub);
          None
      (*x: [[Viewers.view]] match viewer cases *)
      (*| Interactive ->
          interactive frame ctx dh ctype*)
      (*x: [[Viewers.view]] match viewer cases *)
      | Save ->
          Save.interactive (fun _ -> ()) dh;
          None
      (*e: [[Viewers.view]] match viewer cases *)
    with
    (*s: [[Viewers.view]] exn handler 1 *)
    | Failure "too late" -> (* custom for our internal viewers *)
        Document.dclose true dh;
        Document.destroy_log dh false;
        None
    (*x: [[Viewers.view]] exn handler 1 *)
    | Not_found -> 
       (* we don't know how to handle this *)
       ctx#log (s_ "Displaying externally");
       interactive frame ctx dh ctype
    (*e: [[Viewers.view]] exn handler 1 *)
  with 
  (*s: [[Viewers.view]] exn handler 2 *)
  | Http_headers.Invalid_header e ->
      ctx#log (s_ "Malformed type: %s" e);
      unknown frame ctx dh
  | Not_found -> 
      (* Content-type was not defined in the headers *)
      (* and could not be computed from url *)
      unknown frame ctx dh
  (*e: [[Viewers.view]] exn handler 2 *)
(*e: function [[Viewers.view]] *)

(*****************************************************************************)
(* Builtin viewers global *)
(*****************************************************************************)

(*s: constant [[Viewers.builtin_viewers]] *)
let builtin_viewers = ref []
(*e: constant [[Viewers.builtin_viewers]] *)
(*s: function [[Viewers.add_builtin]] *)
let add_builtin t v =
  builtin_viewers := (t,v) :: !builtin_viewers
(*e: function [[Viewers.add_builtin]] *)

(*s: function [[Viewers.reset]] *)
(* ?? -> <> *)
let reset () =
  (* Reset the viewer table *)
  Hashtbl.clear viewers;

  (* Restore the builtin viewers *)
  List.iter (fun (x,y) -> add_viewer x y) !builtin_viewers;

  (*s: [[Viewers.reset()]] setting other viewers *)
  (* Preference settings *)
  Tkresource.stringlist "externalViewers" [] |> List.iter (fun ctype -> 
    try
      let (typ,sub), _pars = Lexheaders.media_type ctype in
      Hashtbl.add viewers (typ,sub) External
    with Http_headers.Invalid_header e ->
      Error.f (s_ "Invalid MIME type %s\n%s" ctype e)
  );
  (*x: [[Viewers.reset()]] setting other viewers *)
  Tkresource.stringlist "savedTypes" [] |> List.iter (fun ctype -> 
    try
      let (typ,sub),_pars = Lexheaders.media_type ctype in
      Hashtbl.add viewers (typ,sub) Save
    with Http_headers.Invalid_header e ->
      Error.f (s_ "Invalid MIME type %s\n%s" ctype e)
  );
  (*e: [[Viewers.reset()]] setting other viewers *)
  ()
(*e: function [[Viewers.reset]] *)
    
(*e: viewers/viewers.ml *)

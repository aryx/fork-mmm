(*
 * Multimedia
 *)
open Printf
open Unix
open Lexing
open Url
open Http_headers
open Document
open Feed
open Www
open Hyper

(* The context given to a viewer *)
(* hyper functions are: "goto", "save", "gotonew" *)
type vparams = (string * string) list
type frame_targets = (string * Widget.widget) list

let frame_adopt w targets = 
  List.map (function 
    | "_self",_ -> "_self", w
    | "_parent", _ -> "_parent", Winfo.parent w
    | s, f -> s, f)
    targets

let frame_fugue targets =
  let rec ff accu = function
      [] -> accu
    | ("_self", _) :: l -> ff accu l
    | ("_parent", _) :: l -> ff accu l
    | p :: l -> ff (p::accu) l
  in
  ff [] targets

type hyper_func = {
  hyper_visible : bool;
  hyper_title : string;
  hyper_func : frame_targets -> Hyper.link -> unit
  }

class  virtual context ((did : Document.document_id), 
		       (v : vparams)) =
 object (self : 'a)

  val base = did
  method base = base

  val viewer_params = v
  method params = viewer_params

  val mutable (*private*) funs = ([] : (string * hyper_func ) list)
  method hyper_funs = funs

  val targets = []

  method goto hlink = self#invoke "goto" hlink
  method gotonew hlink = self#invoke "gotonew" hlink
  method save hlink = self#invoke "save" hlink
  method invoke name hlink =
    try (List.assoc name funs).hyper_func targets hlink
    with Not_found -> ()

  method add_nav (fname, hf) =
    funs <- (fname, hf) :: funs

  method with_target x = {< targets = x >}
  method with_viewer_params x = {< viewer_params = x >}

  (* apply this on a copy ! *)
  method for_embed (vparams: vparams) (newtargets : frame_targets) : 'a =
    (* for debug *)
    let oldtargets = targets in
    let res = 
      match newtargets with 
      | [] -> 
          (* keep exactly the same environment *)
          targets
      | l -> 
          (* assume I'm given new _self and _parent *)
	  l @ frame_fugue targets 
    in
    (* old:
       {< 
       targets = res;
       viewer_params = vparams;       
       >}
    *)
    (* pad: the code below works perfectly but when I want to do 
     * make dotall I get an error because ocamldoc -pp camlp4o (which
     * I need for certain files) has a type error on this file
     * at this line. Indeed when run
     * 
     *   ocamldoc -pp camlp4o -I ../commons -I ../globals -I ../www -I ../http -I ../protocols -I ../retrieve -I /home/pad/packages/Linux/stow/ocaml-3.12/lib/ocaml/camltk viewers.ml
     * 
     * manually camlp4 geneate a weird thing for the code below.
     * so I just introduced those ugly with_target and with_viewer
     * to avoid the pb. UGLY
     * 
     *)
     (self#with_target res)#with_viewer_params vparams

  method in_embed did =
    {< base = did >}

  method virtual log : string -> unit
end



(* The object created/returned by a viewer *)
class  virtual display_info () =
 object (_self : 'a)
  method virtual di_widget : Widget.widget
  method virtual di_abort : unit		(* stop display *)
  method virtual di_destroy : unit		(* die *)
  method virtual di_fragment : string option -> unit	(* for # URIs *)
  method virtual di_redisplay : unit		(* redisplay *)
  method virtual di_title : string		(* some visible title *)
  method virtual di_source : unit 	        (* source viewer *)
  method virtual di_load_images : unit	        (* load images *)
  method virtual di_update : unit	        (* update embedded objects *)
  val mutable di_last_used = !Low.global_time
  method di_last_used = di_last_used
  method di_touch = di_last_used <- !Low.global_time
end

class trivial_display (w, url) =
 object
  inherit display_info ()
  (* val w = w *)
  (* val url = url *)
  method di_widget = w
  method di_abort = ()
  method di_destroy = if Winfo.exists w then Tk.destroy w
  method di_fragment f = ()
  method di_redisplay = ()
  method di_title = Url.string_of url
  method di_source = ()
  method di_load_images = ()
  method di_update = ()
end

let di_compare di di' = di#di_last_used > di'#di_last_used

(* 
 * The default external viewer
 *)

(* Metamail options
   -b : not an RFC822 message
   -z : delete when finished 
   -x : not on a tty 
 *)
let metamail ctype file =
  ignore (Munix.system "metamail -b -z -x -c" [ctype; file] true)

(* Batch version: we transfer everything and then run metamail *)
let extern_batch dh ctype = 
  let outfile = Msys.mktemp "mmm" in
  Document.add_log dh (
    I18n.sprintf "Saving %s\nfor external display with MIME type %s"
	      (Url.string_of dh.document_id.document_url) ctype)
    (fun () -> Msys.rm outfile);
  let endmsg =
    I18n.sprintf "Running metamail with MIME media-type: %s" ctype in
    Save.tofile (metamail ctype) (Decoders.insert dh) outfile endmsg

(* "interactive" version: 
 *    send data to metamail as it arrives, but allow abort
 * NOTE: There are sometimes weird errors when the child dumps core
 *     	 between fork/exec with no apparent reason (on SunOS4.1 only)
 *)
let extern dh ctype =
  let (pin, pout) = pipe() in
  (* children must not keep pout open *)
  Unix.set_close_on_exec pout;
  match Low.fork() with
    0 ->
      dup2 pin stdin; close pin;
      Munix.execvp "metamail" [| "metamail"; "-b"; "-x"; "-c"; ctype |]
  | pid ->  
      close pin;
      let kill () = 
         try Unix.kill pid 2
      	 with Unix_error (e,_,_) ->
	     Log.f (sprintf "Can't kill child (%s)" (Unix.error_message e))
      	      in
      let url = Url.string_of dh.document_id.document_url in
      Document.add_log dh (
	I18n.sprintf "Retrieving %s\nfor external display with MIME type %s"
		     url ctype)
      	kill;

      let red = ref 0 
      and size =   
      	try Http_headers.contentlength dh.document_headers 
      	with Not_found -> 40000 (* duh *)
      and buffer = String.create 4096 in
      dh.document_feed.feed_schedule
      	(fun () ->
	  try
	   let n = dh.document_feed.feed_read buffer 0 4096 in
	   if n = 0 then begin
	       dclose true dh;
	       close pout;
	       Document.end_log dh (I18n.sprintf "End of transmission")
	       end
	   else begin
	     ignore (write pout buffer 0 n);
	     red := !red + n;
	     Document.progress_log dh (!red * 100 / size)
	     end
	  with
	   Unix_error(e,_,_) ->
	     Log.f (sprintf "Error writing to viewer (%s)"
		            (Unix.error_message e));
	     dclose true dh;
	     kill();
	     close pout;
	     Document.destroy_log dh false;
	     Error.default#f (I18n.sprintf "Error during retrieval of %s" url)
	   )

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

(* Definition of an internal viewer *)
type t = 
    media_parameter list -> Widget.widget -> context -> handle 
          -> display_info option

type spec =
    Internal of t
  | External         (* pass to metamail *)
  | Save	     (* always save *)
  | Interactive	     (* ask what to do about it *)

let viewers = Hashtbl.create 17

(* That's for internal viewers only *)
let add_viewer ctype viewer =
  Hashtbl.add viewers ctype (Internal viewer)
and rem_viewer ctype =
  Hashtbl.remove viewers ctype

let rec unknown frame ctx dh =
  match Frx_dialog.f frame (Mstring.gensym "error")
         (I18n.sprintf "MMM Warning")
	 (I18n.sprintf
	   "No MIME type given for the document\n%s"
	   (Url.string_of dh.document_id.document_url))
         (Tk.Predefined "question") 0
	 [I18n.sprintf "Retry with type";
	  I18n.sprintf "Save to file";
	  I18n.sprintf "Abort"] with
   0 ->
    let v = Textvariable.create_temporary frame in
     Textvariable.set v "text/html";
     if Frx_req.open_simple_synchronous (I18n.sprintf "MIME type") v then
       let ctype = Textvariable.get v in
      	 dh.document_headers <- 
      	  ("Content-Type: " ^ ctype) :: dh.document_headers;
      	 view frame ctx dh
     else begin
       Save.interactive (fun _ -> ()) dh;
       None
     end
 | 1 ->
      Save.interactive (fun _ -> ()) dh; None
 | 2 -> dclose true dh; None
 | _ -> assert false (* property of dialogs *)

and interactive frame ctx dh ctype =
  match Frx_dialog.f frame (Mstring.gensym "error")
         (I18n.sprintf "MMM Viewers")
	 (I18n.sprintf
	   "No behavior specified for MIME type\n%s\ngiven for the document\n%s"
	    ctype
	    (Url.string_of dh.document_id.document_url))
         (Tk.Predefined "question") 0
	 [I18n.sprintf "Retry with another type";
	  I18n.sprintf "Display with metamail";
	  I18n.sprintf "Save to file";
	  I18n.sprintf "Abort"] with
  | 0 ->
      let v = Textvariable.create_temporary frame in
      Textvariable.set v "text/html";
      if Frx_req.open_simple_synchronous (I18n.sprintf "MIME type") v then
       	let ctype = Textvariable.get v in
      	dh.document_headers <- 
      	   ("Content-Type: " ^ ctype) :: dh.document_headers;
      	view frame ctx dh
      else begin
       	Save.interactive (fun _ -> ()) dh;
       	None
      end
  | 1 -> extern (Decoders.insert dh) ctype; None
  | 2 -> Save.interactive (fun _ -> ()) dh; None
  | 3 -> dclose true dh; None
  | _ -> assert false (* property of dialogs *)

(* the meat *)
and view frame ctx dh =
  try 
    let ctype = contenttype dh.document_headers in
    let (typ,sub),pars = Lexheaders.media_type ctype in
    try (* Get the viewer *)
      let viewer =
 	try Hashtbl.find viewers (typ,sub)
	with
	  Not_found -> Hashtbl.find viewers (typ,"*")
      in
      match viewer with
      |	Internal viewer ->
	  ctx#log (I18n.sprintf "Displaying...");
      	  viewer pars frame ctx (Decoders.insert dh)
      |	External ->
	  ctx#log (I18n.sprintf "Displaying externally");
	  extern (Decoders.insert dh) (sprintf "%s/%s" typ sub);
	  None
      |	Interactive ->
	  interactive frame ctx dh ctype
      |	Save ->
	  Save.interactive (fun _ -> ()) dh;
	  None
    with
    | Failure "too late" -> (* custom for our internal viewers *)
	dclose true dh;
	Document.destroy_log dh false;
	None
    | Not_found -> 
        (* we don't know how to handle this *)
	ctx#log (I18n.sprintf "Displaying externally");
	interactive frame ctx dh ctype
  with 
  | Invalid_HTTP_header e ->
      ctx#log (I18n.sprintf "Malformed type: %s" e);
      unknown frame ctx dh
  | Not_found -> 
    (* Content-type was not defined in the headers *)
    (* and could not be computed from url *)
    unknown frame ctx dh


let builtin_viewers = ref []
let add_builtin t v =
  builtin_viewers := (t,v) :: !builtin_viewers

let reset () =
  (* Reset the viewer table *)
  Hashtbl.clear viewers;
  (* Restore the builtin viewers *)
  List.iter (fun (x,y) -> add_viewer x y) !builtin_viewers;
  (* Preference settings *)
  let l = Tkresource.stringlist "externalViewers" [] in
  List.iter (fun ctype -> 
    try
      let (typ,sub),pars = Lexheaders.media_type ctype in
      Hashtbl.add viewers (typ,sub) External
    with
      Invalid_HTTP_header e ->
	Error.default#f (I18n.sprintf "Invalid MIME type %s\n%s" ctype e))
    l;
  let l = Tkresource.stringlist "savedTypes" [] in
  List.iter (fun ctype -> 
    try
      let (typ,sub),pars = Lexheaders.media_type ctype in
      Hashtbl.add viewers (typ,sub) Save
    with
      Invalid_HTTP_header e ->
	Error.default#f (I18n.sprintf "Invalid MIME type %s\n%s" ctype e))
    l
    

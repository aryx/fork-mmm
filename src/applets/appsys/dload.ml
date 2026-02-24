(***********************************************************************)
(*                                                                     *)
(*                           Calves                                    *)
(*                                                                     *)
(*          Francois Rouaix, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
open Common
open Fpath_.Operators

let paranoid = ref true  (* selects default capabilities *)

(* This is now available as Dynlink.error_message, but not i18n *)
let dynlinkerror = function
   Dynlink.Not_a_bytecode_file _s ->
     I18n.sprintf "Not a bytecode file"
 | Dynlink.Inconsistent_import s ->
     I18n.sprintf "Inconsistent import: %s" s
 | Dynlink.Unavailable_unit s ->
     I18n.sprintf "Unavailable unit: %s " s
 | Dynlink.Unsafe_file ->
     I18n.sprintf "Unsafe file"
 | Dynlink.Linking_error (s, Dynlink.Undefined_global v) ->
     I18n.sprintf "Error while linking: %s Undefined global: %s" s v
 | Dynlink.Linking_error (s, Dynlink.Unavailable_primitive v) ->
     I18n.sprintf "Error while linking: %s Unavailable primitive: %s" s v
 | Dynlink.Corrupted_interface _s ->
     I18n.sprintf "Corrupted interface"
 | Dynlink.Linking_error (s, Dynlink.Uninitialized_global v) ->
     I18n.sprintf "Error while linking: %s Uninitialized global: %s" s v
(*
 | Dynlink.File_not_found s ->
     I18n.sprintf "Cannot find file %s in search path" s
 | Dynlink.Cannot_open_dll s ->
     I18n.sprintf "Error while loading shared library: %s" s
*)
 | other ->
     Dynlink.error_message other
 

(* The type of entry point functions registered by the applet *)
type applet_callback = Widget.widget -> Viewers.context -> unit

(* The "foreign" module fake cache : what we keep in memory *)
type t = {
  module_address : string;		(* the URL of the bytecode *)
  module_info : string list;		(* headers *)
  module_functions : (string, applet_callback) Hashtbl.t
  } 

type mod_status = 
    Unavailable of string list
  | Rejected of string list
  | Loaded of t

let mod_cache = (Hashtbl.create 53 : (Url.t, mod_status) Hashtbl.t)

let get = Hashtbl.find mod_cache
and iter f = Hashtbl.iter f mod_cache
and remove = Hashtbl.remove mod_cache

(* Register queue
   The queue is used to make a separate hashtbl for each loaded bytecode,
   and thus give some "proper" name space to each applet.
   e.g. an applet has toplevel expressions such as
   let _ = Applets.register 
   NOTE: register must be protected so that it's available only during
   load time. (and even with this protection, it's subject to race
   condition).
 *)

let register_queue = Queue.create()

(* This is the one that we export to applets *)
let register name f =
  Queue.add (name, f) register_queue

(* Create a hashtable of functions
   This is called after loading the bytecode
 *)
let register_flush () =
  let names = Hashtbl.create 37 in
  try
    while true do
      let (name,f) = Queue.take register_queue in
        Hashtbl.add names name f
    done;
    names
  with
    Queue.Empty -> names

(* We need to resynchronize applet evaluation : several <EMBED> may 
   use the same SRC bytecode. The navigator will request us to load
   several times the same bytecode.
 *)

let pending_loads = (Hashtbl.create 37 :
(Url.t, ((string, applet_callback) Hashtbl.t -> unit) Queue.t) Hashtbl.t)

(* add to queue while loading
 *   DO NOT CALL THIS if the bytecode is already loaded
 *   The queue will be flushed whenever the bytecode gets loaded
 *)
let add_pending_applet url cont =
    (* we already have a queue of applets waiting for url *)
    if Hashtbl.mem pending_loads url then false else
      (* this is the first request for this Url *)
      let q = Queue.create() in
      Queue.add cont q; (* add continuation *)
      Hashtbl.add pending_loads url q;
      true

(* Evaluate all pending continuations for this bytecode *)
let flush_pending_applets url ftable =
  try
    let q = Hashtbl.find pending_loads url in
    Hashtbl.remove pending_loads url;
    try
      while true do
	(Queue.take q) ftable
      done
    with
      Queue.Empty -> ()
  with 
    Not_found -> (* url not in pending_loads table. Is that an error ? *)
      ()


(* Dynlink is not reentrant + security requires it to be in critical
   section anyway (to protect capabilities)
   This reference protects:
       1- Dynlink
       2- The queue of functions
 *)

let in_load = ref false

(* Loading of local extensions *)
let load_local (file : string) = 
  Logs.info (fun m -> m "loading %s" file);
  if !in_load then Error.f (I18n.sprintf "Already loading a module")
  else begin
   in_load := true;
   let url = Lexurl.make ("file://"^file) in
   Capabilities.set (Capabilities.local_default url);
   (* prepare the register queue *)
   Queue.clear register_queue; (* this is not needed actually *)
   try
     Dynlink.loadfile_private file;
     Capabilities.reset();
     let funtable = register_flush() in
     in_load := false;
     Hashtbl.add mod_cache url (Loaded 
				  { module_address = Url.string_of url;
				    module_info = [];
				    module_functions = funtable;
				  });
     (* "run" the applets (evaluate continuations that run the entry point) *)
     flush_pending_applets url funtable;
   with e ->
     (* In case of any error here *)
     Capabilities.reset();
     ignore (register_flush());
     in_load := false;
     match e with
     | Dynlink.Error e -> 
	 Error.f (I18n.sprintf "Failed to load Caml module %s\n%s"
      	       	       	         (Url.string_of url) (dynlinkerror e))
     | e ->
	 Error.f (I18n.sprintf "Failed to load Caml module %s\n%s"
      	       	       	         (Url.string_of url)
      	       	       	       	 (Printexc.to_string e))
  end

(* Low-level loading of a foreign bytecode stored in a tmp file *)
let unsafe_load (doc : Document.t) file =
  if !in_load then Error.f (I18n.sprintf "Already loading a module")
  else begin
    in_load := true;
    let url = doc.document_address in
    Capabilities.set 
      ((if !paranoid then Capabilities.strict_default url
        else Capabilities.lenient_default url));
    (* prepare the register queue *)
    Queue.clear register_queue;
    try
      Dynlink.loadfile_private file;
      Capabilities.reset();
      let funtable = register_flush() in
      in_load := false;
      Hashtbl.add mod_cache url (Loaded
				   { module_address = Url.string_of url;
				     module_info = doc.document_headers;
				     module_functions = funtable;
				   });
     (* "run" the applets (evaluate continuations that run the entry point) *)
      flush_pending_applets url funtable;
    with e ->
      Capabilities.reset();
      ignore (register_flush());
      in_load := false;
      match e with
	Dynlink.Error e -> 
	  Error.f (I18n.sprintf "Failed to load Caml module %s\n%s"
      	       	                (Url.string_of url) (dynlinkerror e));
	  failwith "dontkeep"
      | e ->
	  Error.f (I18n.sprintf "Failed to load Caml module %s\n%s"
      	       	       	         (Url.string_of url)
      	       	       	       	 (Printexc.to_string e));
	  failwith "dontkeep"
  end
      

let ask url =
  0 = Frx_dialog.f Widget.default_toplevel (Mstring.gensym "accept")
       "MMM Question"
       (I18n.sprintf  "Unsigned bytecode file %s" (Url.string_of url))
       (Predefined "question") 1
       ["Accept"; "Reject"]


(* In which form is the applet ?
 * We'd like to push a solution with a single MIME type
 *   application/x-caml-applet
 *   and an attribute specifying the encoding : encoding  = source/bytecode
 * However:
 *  1- it's difficult to set up MIME attributes on servers (at least Apache)
 *  2- a navigator might not pass this information to a plugin
 * Thus:
 *  in case the attribute is not present, we try to compute it from
 *  the URL suffix, and magic numbers.
 *)


type applet_kind =
    Bytecode of bool (* signed ? *)
  | Source

(* may raise Invalid_HTTP_header e *)
let applet_kind (doc : Document.t) file =
  let kind () =
    (* TODO: pass caps *)
    let ic = open_in file
    and buf : bytes = Bytes.create 8 in
    really_input ic buf 0 8;
    close_in ic;
    if Bytes.to_string buf = "Caml1999" then Bytecode false else 
    if Bytes.sub_string buf 0 2 = "(*" || Bytes.sub_string buf 0 5 = "open " then Source
    else (* check suffix *)
      match Mstring.get_suffix (Url.string_of doc.document_address) with
	"ml" -> Source
      | "cmo" | "cma" -> Bytecode false
      | "pgp" | "scmo" -> Bytecode true
      | _ -> (* assume pgp signed... *) Bytecode true
  in
  match Lexheaders.media_type (Http_headers.contenttype doc.document_headers) with
  | ("application", "x-caml-applet"), [] -> (* do magic number trick *)
      kind ()
  | ("application", "x-caml-applet"), l -> 
      begin
	try 
	  match String.lowercase_ascii (List.assoc "encoding" l) with
	    "source" -> Source
	  | "signed-bytecode" -> Bytecode true
	  | "bytecode" -> Bytecode false
	  | _ -> kind ()
      	with
	  Not_found -> kind ()
      end
  | _, _ ->
      failwith "dontkeep"

(* Load a foreign bytecode *)
let load (doc : Document.t) =
  let url = doc.document_address in
  Logs.info (fun m -> m "dynamic loading doc %s" (Url.string_of url));
  (* do we have it already loaded ? TODO: check last modified *)
  try
    ignore (Hashtbl.find mod_cache url) (* then forget it *)
  with
    Not_found ->
     (* actually load it from some file. [remove] says if we should
	remove the file after loading. If the file is from the cache,
	it is NOT our responsability to remove it
      *)
      let file, remove = 
       match doc.document_data with
       | FileData(file,_) -> file, false
       | MemoryData buf ->
	  let file = Msys.mktemp "mmmbuf" in
	  let oc = open_out file in
	  output_string oc (Ebuffer.get buf);
	  close_out oc;
	  Fpath.v file, true
      in
      try match applet_kind doc !!file with
	Source ->
	  (* the mmmc script is part of the distribution *)
	  let byt = Msys.mktemp "apcode" in
	  let cmd = spf "mmmc %s %s" !!file byt in
	  begin match Sys.command cmd with
	    0 -> 
	      unsafe_load doc byt; 
	      Msys.rm byt;
	      if remove then Msys.rm !!file
	  | _n -> 
	      Error.f (I18n.sprintf "Can't compile applet %s"
			 (Url.string_of url));
	      Msys.rm byt;
	      if remove then Msys.rm !!file
	  end
      |	Bytecode true ->
	   begin match Pgp.check (Url.string_of url) !!file with
	     Some clear ->
	       unsafe_load doc clear;
	       Msys.rm clear;
	       if remove then Msys.rm !!file
	   | None -> (* was refused or malformed *)
	       if remove then Msys.rm !!file;
	       failwith "dontkeep"
	   end
      |	Bytecode false ->
	  if ask url then begin
	    unsafe_load doc !!file;
	    if remove then Msys.rm !!file;
	  end else begin
	    if remove then Msys.rm !!file;
	    failwith "dontkeep"
	  end
     with
       Failure "dontkeep" ->
	 if remove then Msys.rm !!file;
	 Hashtbl.add mod_cache url (Rejected doc.document_headers);
	 Error.f (I18n.sprintf "%s was rejected" (Url.string_of url))

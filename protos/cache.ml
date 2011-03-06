(* Document caching (in memory !) *)
open Printf
open Unix
open Url
open Www
open Hyper
open Document
open Feed
open Http_headers

let debug = ref false
let history_mode = ref false
  (* history mode means that we keep only the documents present in some
     navigator window. This mode is meant to be used in conjunction with
     a caching proxy *)

let max_lastused = 100000000000.0

(* The max values refer documents kept in memory *)
let max_documents = ref 30
and cleann = ref 5
and current = ref 0

(* A list of operations to do when we remove a document from the cache. *)
let cutlinks = ref []

type cache_fill = {
  cache_write : string -> int -> int -> unit;
  cache_close : unit -> unit
 }

(* A cache entry *)
type entry = {
  mutable cache_document : document;
  mutable cache_pending : bool;
  cache_condition : Condition.t;
  mutable cache_lastused : float
      (* cache_lastused is specified as max_int (0x3fffffff) when we don't
       * want the entry to be flushed. This will break around
       * Sat Jan 10, 2004 13:37 GMT on 32 bits machines
       *) (* JPF: it is now float! max_int -> max_float *)
  }

exception DontCache

let memory = ref ([] : (document_id * entry) list)

(* Debugging *)
let postmortem () =
  Log.f (sprintf "Cache size(max): %d(%d)" !current !max_documents);
  List.iter 
    (fun (did, entry) ->
      Log.f (sprintf "%s(%d) %s"
	       (Url.string_of did.document_url)
	       did.document_stamp
	       (match entry.cache_document.document_data with
		 MemoryData _ -> "in memory"
	       | FileData (f,true) -> f
	       | FileData (f,false) -> "fake " ^f));
      List.iter	(fun h -> Log.f (sprintf "%s" h))
 	(List.rev entry.cache_document.document_info);
      if entry.cache_pending then Log.f "pending ";
      Log.f (sprintf "Last used: %f" entry.cache_lastused);
      Log.f "")
    !memory


(* Find a document*)
let find did =
  let entry = List.assoc did !memory in
    entry.cache_lastused <- Unix.time();
    if entry.cache_pending then Condition.wait entry.cache_condition;
    entry.cache_document

(* Kills a document: stop and destroy all its dinfo
 * The caller is responsible for possible removing the document itself
 * from the memory.
 *)
let internal_kill did e =
   (* Remove pointers to in-lined images and other goodies *)
   List.iter (fun f -> f did) !cutlinks

let make_room () =
  if !debug then Log.f "Trying to make room in cache";
  (* Sort.list according to lru *)
  memory := Sort.list 
      	     (fun (_,e) (_,e') -> e.cache_lastused < e'.cache_lastused)
	     !memory;
  (* if the more recent entry has lu max_lastused, then we have to augment 
     the cache, since this means that only pending connexions are
     in the cache *)
  begin match !memory with
    [] -> ()
  | (_,e)::l ->
     if e.cache_lastused = max_lastused then max_documents := !max_documents + 5
     else (* cleanup the oldests entries *)
       let rec rem1 n l = 
       	if n = 0 then l
	 else match l with
	   [] -> []
	| (did, e)::l ->
	     internal_kill did e;
	     decr current;
	     rem1 (n-1) l
        in
	  memory := rem1 !cleann !memory
  end;
  if !debug then begin
     Log.f (sprintf "Cache size(max): %d(%d)" !current !max_documents);
     Log.f "Cache contents:";
     postmortem()
  end


(* Remove the document source. *)
let finalize = function
   FileData (f, true) -> Msys.rm f
 | _ -> () (* gc ! *)

(* kill: removes a document from the cache
 *   Used by Reload. It can fail to find url in memory !
 *   It can also be used to remove something from the file cache
 *)
let kill_entry did e =
  if !debug then
    Log.f (sprintf  "Killing cache entry %s(%d)"
	     (Url.string_of did.document_url)
	     did.document_stamp);
  internal_kill did e;	(* kill dinfo in all windows *)
  finalize e.cache_document.document_data;	(* remove source *)
  memory := Mlist.except_assoc did !memory;
  decr current

let kill did =
  try
    let e = List.assoc did !memory in
      kill_entry did e
  with
    Not_found -> ()

(* Add a new entry *)
let add did doc =
  if !debug then
     Log.f (sprintf  "Adding new cache entry %s(%d) %s"
      	       	     (Url.string_of did.document_url)
	             did.document_stamp
		     (match doc.document_data with
			MemoryData _ -> "in memory"
		      | FileData (f,true) -> f
		      | FileData (f,false) -> "fake " ^f));
  (* Kill the previous entry, if any [for update] *)
  kill did;
  (* Because of frames (not kept in history), we must make room even
   * in history mode
   *)
  if (*not !history_mode && *)!current >= !max_documents then make_room()
  else if !debug then
    Log.f (sprintf "Cache size(max): %d(%d)" !current !max_documents);
  incr current;
  memory :=(did,
	    {cache_document = doc;
	     cache_pending = true;
	     cache_lastused = max_lastused;
	     cache_condition = Condition.create()})
	    :: !memory



(* Pending documents should never be removed from the cache *)
(* since they have lu = max_lastused *)
let finished did =
  if !debug then
     Log.f (sprintf "%s completed" (Url.string_of did.document_url));
  try
    let entry = List.assoc did !memory in
      entry.cache_lastused <- Unix.time();
      entry.cache_pending <- false;
      Condition.set entry.cache_condition
  with
    Not_found -> ()

let touch did =
  try
    let entry = List.assoc did !memory in
      entry.cache_lastused <- max (Unix.time()) entry.cache_lastused
  with
    Not_found -> ()

(* Patch the headers of an existing entry *)
let patch did headers =
  try
    let entry = List.assoc did !memory in
    let newd = {
      document_address = entry.cache_document.document_address;
      document_data = entry.cache_document.document_data;
      document_info = merge_headers entry.cache_document.document_info headers
      } in
     entry.cache_document <- newd;
    entry.cache_lastused <- max (Unix.time()) entry.cache_lastused
  with
    Not_found -> () (* is this an error ? *)

let init () =
  let initurl = Lexurl.make (Version.initurl (Lang.lang ())) in
  let b = Ebuffer.create 128 in
  Ebuffer.output_string b (Version.html (Lang.lang ()));
  memory := [
    {document_url = initurl;
     document_stamp = no_stamp}, 
    {
    cache_document = {document_address = initurl;
      	       	      document_data = MemoryData b;
		      document_info = ["Content-Type: text/html"]};
    cache_pending = false;
    cache_condition = Condition.create();
    cache_lastused = max_lastused}];
  current := 1


(* Cache savers *)
let tofile dh =
  let f = Msys.mktemp "mmmcache" in
  let oc = open_out_bin f in
    FileData (f,true), 
      {cache_write = output oc;
       cache_close = (fun () -> close_out oc)}

let tobuffer dh =
  let b = Ebuffer.create 1024 in
    MemoryData b, {cache_write = Ebuffer.output b;
      	           cache_close = (fun () -> ())}

let discard =
    {cache_write = (fun buf offs len -> ());
     cache_close = (fun () -> ())}

(* Pseudo-caching for documents that can be obtained from the local
   file system. Relies on trailing slash for directories !
 *)

let dummy dh =
  let url = dh.document_id.document_url in
   match url.protocol with
     FILE -> 
       begin match url.path with
	 None -> tobuffer dh
       | Some "" -> tobuffer dh
       | Some p ->
	  if p.[String.length p - 1] = '/' then tobuffer dh
	  else FileData ("/"^p, false), discard
       end
   | _ -> raise DontCache

let replace = function
   MemoryData b ->
    Ebuffer.reset b; 
    {cache_write = Ebuffer.output b; cache_close = (fun () -> ())}
 | FileData (f, _) ->
  let oc = open_out_bin f in
    {cache_write = output oc;
     cache_close = (fun () -> close_out oc)}

(* Wrap a feed with cache saving *)
let wrap c dh = 
  let wfeed = {
    feed_read = 
      (fun buf offs len ->
      	 let r = dh.document_feed.feed_read buf offs len in
	 if r <> 0 then c.cache_write buf offs r;
	 r);
    feed_schedule = dh.document_feed.feed_schedule;
    feed_unschedule = dh.document_feed.feed_unschedule;
    feed_close =
      (fun () ->
      	 dh.document_feed.feed_close();
	 c.cache_close();
	 finished dh.document_id);
    feed_internal = dh.document_feed.feed_internal
    }
  in
  {document_id = dh.document_id;
   document_referer = dh.document_referer;
   document_status = dh.document_status;
   document_headers = dh.document_headers;
   document_feed = wfeed;
   document_fragment = dh.document_fragment;
   document_logger = dh.document_logger
  }

(* Obtain a dh from a cache entry *)
(* This is stupid: to display a source that we have in the cache, we must
 * save it to disk in order to get a file descriptor...
 *)

let fd_of_doc doc =
  match doc.document_data with
    MemoryData buf ->
      let f = Msys.mktemp "mmmbuf" in
      let oc = open_out f in
      output_string oc (Ebuffer.get buf);
      close_out oc;
      let fd = openfile f [O_RDONLY] 0 in
      Msys.rm f;
      fd
  | FileData (f,_) -> openfile f [O_RDONLY] 0

let make_handle wwwr doc =
  { document_id = { document_url = wwwr.www_url; document_stamp = no_stamp};
    document_referer = wwwr.www_link.h_context;
    document_status = 200;
    document_headers = doc.document_info;
    document_feed = Feed.of_fd (fd_of_doc doc);
    document_fragment = wwwr.www_fragment;
    document_logger = tty_logger}

(* The same, if we kept the old dh *)
let renew_handle dh =
  let did = dh.document_id in
  let doc = find did in
  { document_id = dh.document_id;
    document_referer = dh.document_referer;
    document_status = dh.document_status;
    document_headers = doc.document_info;
    document_feed = Feed.of_fd (fd_of_doc doc);
    document_fragment = dh.document_fragment;
    document_logger = dh.document_logger}


(* Same for embedded objects (but we don't have wwwr handy) *)
let make_embed_handle doc =
  let fd =
    match doc.document_data with
      MemoryData buf ->
	let f = Msys.mktemp "mmmbuf" in
	  let oc = open_out f in
	    output_string oc (Ebuffer.get buf);
	    close_out oc;
	let fd = openfile f [O_RDONLY] 0 in
	  Msys.rm f;
	  fd
    | FileData (f,_) -> openfile f [O_RDONLY] 0
  in
    {document_id = 
	{ document_url = doc.document_address; document_stamp = no_stamp};
     document_referer = None;
     document_status = 200;
     document_headers = doc.document_info;
     document_feed = Feed.of_fd fd;
     document_fragment = None;
     document_logger = tty_logger}

   
let cleanup () =
  List.iter 
    (fun (did, entry) ->
      match entry.cache_document.document_data with
      	FileData (f, true) -> Msys.rm f
      | _ -> ())
    !memory

let _ = at_exit cleanup

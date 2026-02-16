(*s: ./gui/gcache.ml *)
open Printf
open Document
open Viewers

(*s: constant Gcache.debug *)
(* Cache by "widget unmapping"
 *  For each navigator, we keep the list of displayed documents
 *)

let debug = ref false
(*e: constant Gcache.debug *)

(*s: constant Gcache.max_keep *)
let max_keep = ref 5
  (* maximum number of cached widget in a given window *)
(*e: constant Gcache.max_keep *)

(*s: constant Gcache.table *)
let table = (Hashtbl.create 37 :
              (int, (document_id * display_info) list ref) Hashtbl.t)
(*e: constant Gcache.table *)

(*s: function Gcache.get_nav *)
let get_nav hkey =
  try
    Hashtbl.find table hkey
  with
    Not_found ->
      let r = ref [] in
       Hashtbl.add table hkey r;
       r
(*e: function Gcache.get_nav *)

(*s: function Gcache.find *)
(* Find a document in a given window
 * Called by the navigator when attempting to display a new request.
 * Also called by back/forward navigation in the history
 *)
let find hkey did =
  let r = get_nav hkey in
  List.assoc did !r
(*e: function Gcache.find *)

(*s: function Gcache.nocache *)
(* History mode: when we remove a document from the gcache, and that it
   was its only displayed instance, then we must also remove it from cache
 *)
let nocache did =
 if !debug then Log.f
    (sprintf "Removing %s from cache" (Url.string_of did.document_url));
 let shared = ref false in
  Hashtbl.iter (fun _key dis -> if List.mem_assoc did !dis then shared := true)
    table;
 if not !shared then Cache.kill did
 else  
   if !debug then Log.f "Don't, it's shared"
(*e: function Gcache.nocache *)

(*s: function Gcache.remove *)
(* Removes a given dinfo for a cached document
 *   used when adding in the middle of the history
 *)
let remove hkey did =
  if !debug 
  then Log.f (sprintf "Removing %s in window %d" 
                 (Url.string_of did.document_url) hkey);
  try
    let r = Hashtbl.find table hkey in
    let di = List.assoc did !r in
    di#di_abort;
    di#di_destroy;
    r := Mlist.except_assoc did !r;
    if !Cache.history_mode 
    then nocache did
  with Not_found -> 
    Log.debug "Gcache.remove failed !"
(*e: function Gcache.remove *)

(*s: function Gcache.displace *)
(* Removing only to redisplay (update) *)
let displace hkey did =
  if !debug then Log.f
    (sprintf "Displacing %s in window %d" (Url.string_of did.document_url) hkey);
  try
    let r = Hashtbl.find table hkey in
    let di = List.assoc did !r in
     di#di_abort;
     di#di_destroy;
     r := Mlist.except_assoc did !r;
  with
     Not_found -> Log.debug "Gcache.remove failed !"
(*e: function Gcache.displace *)

(*s: function Gcache.add *)
(* Add a new display_info for a document in the cache *)
let add hkey did di =
  try
    let r = Hashtbl.find table hkey in
     r := (did, di) :: !r;
    (* the problem is to find the correct ones to delete, because we are
       not sure that the older are really the older in history. Well.
     *)
    if List.length !r > !max_keep then
      let l = List.sort (fun (_,di) (_,di') -> di_compare di di') !r in
      let fluff = Mlist.tln l !max_keep in
    List.iter (fun (did,_) -> remove hkey did) fluff
  with
    Not_found -> ()
(*e: function Gcache.add *)


(*s: function Gcache.kill *)
(* A window is being destroyed: kill all visible instances
 *  Note: there could be a document still being retrieved and displayed,
 *  but not present in the history. 
 *)
let kill hkey =
 if !debug then Log.f (sprintf "Killing gcache for nav %d" hkey);
 let r = get_nav hkey in
  List.iter (fun (_did, di) -> di#di_abort) !r;
  if !Cache.history_mode then begin
     let fluff = !r in
       r := []; (* so that we don't find them again *)
       List.iter (fun (did, _) -> nocache did) fluff
  end;
 Hashtbl.remove table hkey
(*e: function Gcache.kill *)


(*s: function Gcache.postmortem *)
let postmortem () =
  Hashtbl.iter (fun key dis ->
      Log.f (sprintf "Navigator %d" key);
      List.iter (fun (did,_) ->
     Log.f (sprintf "%s(%d)"
          (Url.string_of did.document_url)
          did.document_stamp))
         !dis)
   table
(*e: function Gcache.postmortem *)


(*s: function Gcache.sorry *)
(* If the normal cache gets full, we might *have* to destroy documents
 * that are visible. In that case, kill the gcache as well, so that
 * we don't get strange phenomenons such as image disappearing, ...
 *)

let sorry did =
  Hashtbl.iter (fun key dis -> 
    if List.mem_assoc did !dis then remove key did) table
(*e: function Gcache.sorry *)


(*s: toplevel Gcache._1 *)
let _ =
  Cache.cutlinks := sorry :: !Cache.cutlinks
(*e: toplevel Gcache._1 *)
(*e: ./gui/gcache.ml *)

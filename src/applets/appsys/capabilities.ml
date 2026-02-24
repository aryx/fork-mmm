(*s: capabilities.ml *)
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

(* 
 A simple capability manager : each applet get its own list of access rights.

 The capabilities are given *at load-time* for a bytecode, using the
 current_capa reference and its accessorts get/set/reset. current_capa
 is non-empty only during load-time. An attempt to "get" after load-time
 will result in a Not_found exception. 
 (Dynamic loading must be in a critical section anyway, since Dynlink
  would not support interleaved loading. The only reason for which
  a "context switch" would occur is when there is toplevel expression
  in the applet that causes Tk to enter its even loop (remember we
  don't have true threads here).)

 NOTE: about a possible race condition (an active applet calling
       Capabilities.get() during its execution, while we are
       loading another applet and current_capa is Some c)

  this race condition may happen only if some toplevel expression
  in the applet being loaded yields to the Tk event loop (because
  otherwise the loading is completely synchronous).

  Suppose this happens.

  The active applet now has a handle to the capabilities (type t) 
  that were granted to the applet being loaded. The only possible
  operation is to require more rights for this new applet. The
  request will correctly appear to the user.

  But since capabilites are stored in the closures of dangerous
  functions at load-time (by functor application), there is no
  way for the active applet to benefit from the rights of the
  applet being loaded.
  (assuming that loading of applets can't be interleaved, which is
   true by virtue of "in_load")

 NOTE: about checking rights
  When checking the rights on a given "ressource description" (e.g.
  a filename or an URL), we must be sure that this ressource cannot
  be modified by the applet between the moment we check it and the
  moment we actually use it.

*)

(*s: exception [[Capabilities.Denied]] *)
exception Denied (* access denied *)
(*e: exception [[Capabilities.Denied]] *)

(*s: type [[Capabilities.mode]] *)
type mode = Fixed | Extend | Temporary (* extension mode for access rights *)
(*e: type [[Capabilities.mode]] *)

(* Fixed means that the only rights are the initial rights.
   Extend means that a right may be requested, and if granted,
     will be granted for any further requests.
   Temporary means that a right may be granted temporarily, but must be
     granted again if requested for later.
 *)

(*s: function [[Capabilities.string_of_mode]] *)
let string_of_mode = function
    Fixed -> ""
  | Extend -> I18n.sprintf "repeated"
  | Temporary -> I18n.sprintf "temporary"
(*e: function [[Capabilities.string_of_mode]] *)

(* Since we don't have subtyping of signatures, there is no easy way
   to make access rights extensible. Even if we export separate functions
   for requesting each kind of access right, we still get different
   signatures. Thus, it doesn't make any difference for the applet author
   if we export this type or if we export construction functions.
   For example, HTMLDisplay is not available for Calves applets, but
   we do export it. In the future, we would have to use the Safe$(VERSION)
   mechanism to ensure backward compatibility...
 *)
(*s: type [[Capabilities.right]] *)
type right =
   FileR of string			(* read access to files *)
 | FileW of string			(* write acces to files *)
 | DocumentR of string			(* read access to URLs *)
    (* Document read access affects decoders, embedded viewers, as well as
       the general retrieval mechanism. It means that the applet has
       access to the document body. (Navigation, that is triggering 
       retrieval/display of documents is always available, since inspection
       of URL is essentially useless).
     *)
 | HTMLDisplay
   (* HTML display machine access : this is very liberal; if granted, it
      means that the applet has access to *all* retrieved HTML documents
    *)
 | Internals
(*e: type [[Capabilities.right]] *)

module Rights = Set.Make(struct type t = bool * right
                               let compare = compare end)
  (* the flag indicates that regexp matching should be used *)


(* The information that will be put in the argument structure of functors
   producing modules when access control is needed.
   This is the easiest way to stick this value in lots of closures at the
   same time, as well as keeping signatures of modules identical to their
   original version (without access control).
   IMPORTANT NOTE:  the "who" field, an Url.t, is mutable (it would also
   be mutable in string form).
   Hence, we want to make a copy of it if we want to give it to the applet,
   and keep our own version private so that the applet can't change it under
   our feet... 
   *)

(*s: type [[Capabilities.t]] *)
type t = {
  mutable mode : mode;
  mutable rights : Rights.t;
  who: Url.t; (* where this applet was loaded from. *)
  }
(*e: type [[Capabilities.t]] *)

(*
 * Various constructors 
 *)

(*s: function [[Capabilities.local_default]] *)
(* For applets loaded from disk, we basically authorize access to any
   HTML document and browser extensions
  *)
let local_default url = {
  mode = Extend;
  rights = 
    List.fold_right Rights.add 
      [true, HTMLDisplay;
       true, DocumentR ".*"]
      Rights.empty;
  who = url;
  }
(*e: function [[Capabilities.local_default]] *)

(*s: function [[Capabilities.lenient_default]] *)
(* For signed applets, we start from an empty set of rights, but allow
   right extension requests *)
let lenient_default url = {
  mode = Extend;
  rights = Rights.empty;
  who = url;
  }
(*e: function [[Capabilities.lenient_default]] *)

(*s: function [[Capabilities.strict_default]] *)
(* For unsigned applets, but then, these are inherently unsafe,
   so there's no point defining any policy *)
let strict_default url = {
  mode = Temporary;
  rights = Rights.empty;
  who = url;
  }
(*e: function [[Capabilities.strict_default]] *)


(* This is the crucial reference from which an applet should get its
   access rights at load-time (and not later) *)
(*s: constant [[Capabilities.current_capa]] *)
let current_capa = ref None
(*e: constant [[Capabilities.current_capa]] *)

(*s: function [[Capabilities.set]] *)
(* Call this to init to some value BEFORE loading some bytecode *)
let set h = current_capa := Some h
(*e: function [[Capabilities.set]] *)
(*s: function [[Capabilities.reset]] *)
(* Call this AFTER loading the bytecode. *)
let reset () = current_capa := None
(*e: function [[Capabilities.reset]] *)


(*
 * Various checks
 *)

(*s: function [[Capabilities.check_FileR]] *)
(* For file names, we must of course remove dots before doing checks.
 * Also, we must be aware that a control context-switch between the
 * return of this function and the use of its result may be the occasion
 * for the applet to physically change the filename string. This race
 * condition may lead to security breach.
 * The only proper usage is to first make a copy of the string, and then
 * check the rights on the copy and use the copy if access is granted.
 * NOTE: even with this precaution, we still have the infamous Unix access()
 * race condition (although here it's probably more difficult to exploit).
 *)

let check_FileR capa s =
  let s = Lexpath.remove_dots s in
  try
    Rights.iter (function
      | true, FileR r when Str.string_match (Str.regexp r) s 0 ->
      failwith "yes"
      | false, FileR r when r = s ->
      failwith "yes"
      | _ -> ())
     capa.rights;
    false
  with
    Failure "yes" -> true
  | _ -> false (* be conservative... *)
(*e: function [[Capabilities.check_FileR]] *)

(*s: function [[Capabilities.check_FileW]] *)
let check_FileW capa s =
  let s = Lexpath.remove_dots s in
  try
    Rights.iter (function
       true, FileW r when Str.string_match (Str.regexp r) s 0 ->
      failwith "yes"
      | false, FileW r when r = s ->
      failwith "yes"
      | _ -> ())
     capa.rights;
    false
  with
    Failure "yes" -> true
  | _ -> false (* be conservative... *)
(*e: function [[Capabilities.check_FileW]] *)

(*s: function [[Capabilities.check_DocumentR]] *)
(* Do we need to remove ../ here ? *)
let check_DocumentR capa s =
  try
    Rights.iter (function
       true, DocumentR r when Str.string_match (Str.regexp r) s 0 ->
      failwith "yes"
      | false, DocumentR r when r = s ->
      failwith "yes"
      | _ -> ())
     capa.rights;
    false
  with
    Failure "yes" -> true
  | _ -> false (* be conservative... *)
(*e: function [[Capabilities.check_DocumentR]] *)

(*s: function [[Capabilities.check_HTMLDisplay]] *)
let check_HTMLDisplay capa _ =
  try
    Rights.iter (function
       _, HTMLDisplay -> failwith "yes"
      | _ -> ())
     capa.rights;
    false
  with
    Failure "yes" -> true
  | _ -> false (* be conservative... *)
(*e: function [[Capabilities.check_HTMLDisplay]] *)

(*s: function [[Capabilities.check_Internals]] *)
let check_Internals capa _ =
  try
    Rights.iter (function
       _, Internals -> failwith "yes"
      | _ -> ())
     capa.rights;
    false
  with
    Failure "yes" -> true
  | _ -> false (* be conservative... *)
(*e: function [[Capabilities.check_Internals]] *)


(* GUI *)
open Tk

(* Ask for a capability
 *  isregexp is true when the applet requires caps during load-time
 *   we then simply popup the question
 *  otherwise we check if it's been granted or ask the user (unless
 *  mode is Fixed)
 *)
(*s: type [[Capabilities.question]] *)
type 'a question = {
  check_right: t -> string -> bool;
  make_right : string -> right;
  question_simple: (string -> string -> 'a, unit, string) format;
  question_regexp: (string -> string -> 'a, unit, string) format
  }
(*e: type [[Capabilities.question]] *)


(*s: type [[Capabilities.cright]] *)
type cright =
  CFileR | CFileW | CDocumentR | CHTMLDisplay | CInternals
(*e: type [[Capabilities.cright]] *)

(* The argument passed to make_right must be a value owned by US
   (that is, it must not be mutated by the applet)
 *)
(*s: constant [[Capabilities.table]] *)
let table = [
  CFileR,
  { check_right = check_FileR;
    make_right = (fun s -> FileR s);
    question_simple = "Grant %s read access to the file\n%s";
    question_regexp = "Grant %s read access to files matching\n%s"};
  CFileW,
  { check_right = check_FileW;
    make_right = (fun s -> FileW s);
    question_simple = "Grant %s write access to the file\n%s";
    question_regexp = "Grant %s write access to files matching\n%s"};
  CDocumentR,
  { check_right = check_DocumentR;
    make_right = (fun s -> DocumentR s);
    question_simple = "Grant %s read access to document\n%s";
    question_regexp = "Grant %s read access to documents matching\n%s"};
  CHTMLDisplay,
  { check_right = check_HTMLDisplay;
    make_right = (fun _ -> HTMLDisplay);
    question_simple = "Grant %s access to HTML display machine\n%s";
    question_regexp = "Grant %s access to HTML display machine\n%s"};
  CInternals,
  { check_right = check_Internals;
    make_right = (fun _ -> Internals);
    question_simple = "Grant %s access to MMM internals\n%s";
    question_regexp = "Grant %s access to MMM internals\n%s";}
]
(*e: constant [[Capabilities.table]] *)

(*s: function [[Capabilities.get_question]] *)
let get_question = function
  | FileR s -> s, List.assoc CFileR table
  | FileW s -> s, List.assoc CFileW table
  | DocumentR s -> s, List.assoc CDocumentR table
  | HTMLDisplay -> "", List.assoc CHTMLDisplay table
  | Internals -> "", List.assoc CInternals table
(*e: function [[Capabilities.get_question]] *)

(* This is the function available for Safe libraries *)
(*s: function [[Capabilities.ask]] *)
(* REMEMBER TO MAKE COPIES OF ARGUMENT IF MUTABLE *)
let ask capa r =
  let param, q = get_question r
  and mode = string_of_mode capa.mode in
  if q.check_right capa param then true (* already granted *)
  else begin
    let title =
      I18n.sprintf "Security check for %s" (Url.string_of capa.who) in
    let question = I18n.sprintf q.question_simple mode param in
    let granted = Error.choose (I18n.sprintf "%s\n%s\n" title question) in
    if granted && capa.mode = Extend then
      capa.rights <- Rights.add (false, q.make_right param) capa.rights;
    granted
  end
(*e: function [[Capabilities.ask]] *)

(*s: function [[Capabilities.require_capa]] *)
(* Here, we make copies ourselves *)
let require_capa capa r =
  let param, q = get_question r
  and mode = string_of_mode capa.mode in
  (* old: let param = String.copy param in, but string are immutable now *)
  if capa.mode = Extend then begin
    let title =
      I18n.sprintf "Security Rights asked by %s" (Url.string_of capa.who) in
    let question = I18n.sprintf q.question_regexp mode param in
    let granted = Error.choose (I18n.sprintf "%s\n%s\n" title question) in
    if granted then
      capa.rights <- Rights.add (false, q.make_right param) capa.rights;
    granted
  end
  else (* not authorized to extend rights. Only "ask" will work. *)
    false
(*e: function [[Capabilities.require_capa]] *)


(* TODO: we would also need a "security editor" *)


(*s: function [[Capabilities.get]] *)
(*
 * This is exported to applets
 *)
let get () =
  match !current_capa with
    None -> raise Not_found
  | Some h -> h
(*e: function [[Capabilities.get]] *)

(*s: function [[Capabilities.require]] *)
let require capa l =  
  List.fold_right (&&) 
    (List.map (require_capa capa) l)
    true
(*e: function [[Capabilities.require]] *)
(*e: capabilities.ml *)

(***********************************************************************)
(*                                                                     *)
(*                    Objective Caml Applets API                       *)
(*                                                                     *)
(*          Francois Rouaix, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Safe browser internals *)

(* Miscellaneous utilities : nothing really interesting. But then, it's safe *)
module Mstring : sig
  val split_str : (char -> bool) -> string -> string list
  val get_suffix : string -> string
  val gensym : string -> string
end

module Mlist : sig
  val rev_do_list : ('a -> unit) -> 'a list -> unit
end

module Feed : sig
  type internal
  type t = {
    feed_read : string -> int -> int -> int;
    feed_schedule : (unit -> unit) -> unit;
    feed_unschedule : unit -> unit;
    feed_close : unit -> unit;
    feed_internal : internal  
    }
  (* A Feed is a handle to the contents of a document being loaded.
     As loading should be kept asynchronous, the proper usage of a feed is
     to feed_schedule a callback that does feed_read.
     feed_read returns 0 when EOF is reached.
     A synchronous feed_read (outside the invocation of a callback installed
     by feed_schedule) might block the applet/navigator indefinitely.
   *)
  end

(* Various dialog boxes *)
module Error : sig
  val f : string -> unit
    (* [f msg] displays an error message [msg] in a dialog box and waits
       for the user to click Ok *)
  val ok : string -> unit
    (* [ok msg] displays the warning or simple notification [msg] in a
       dialog box and waits for the user to click Ok *)
  val choose: string -> bool
    (* [choose msg] displays [msg] in a dialog box, and waits for the user
       to click on Ok or Cancel. Returns true for Ok, false for Cancel *)
  val ari : string -> int
    (* [ari msg] displays [msg] in a dialog box, and waits for the user
       to click on Abort/Retry/Ignore. Returns 0/1/2 respectively *)

  class t : (Widget.widget) -> object
   method f : string -> unit
   method ok : string -> unit
   method choose : string -> bool
   method ari : string -> int
  end

  end


module Url : sig
  type t 
  val string_of : t -> string
  end

module Lexurl : sig
  val make : string -> Url.t
  end

module Hyper : sig
  type link_method = 
     GET 
   | HEAD
   | POST of string

  type link = {
    h_uri : string;
    h_context: string option;
    h_method : link_method;		(* default is GET *)
    h_params : (string * string) list
    }

  type link_error =
      LinkResolve of string
    | UrlLexing of string * int

  exception Invalid_link of link_error

  val urlconcat: Url.t -> string -> string
     (* [urlconcat url relurl] resolves the relative URL [relurl] in the
	 context of the URL [url]
	Doesn't handle fragments
      *)
  val string_of : link -> string
    (* make an abolute URI (including fragment) from link 
       raises Invalid_link(msg) *)

end

module Www : sig
(*
 * Requests
 *)

type request =  { 
    www_link : Hyper.link;        (* the link that produced this request *)
    www_url : Url.t;	          (* parsed version *)
    www_fragment : string option; (* because viewer is passed down *)
    mutable www_auth : (string * string) list;  (* basic auth *)
    mutable www_headers : string list;		  (* additional headers *)
    mutable www_logging : string -> unit;	  (* logging *)
    mutable www_error : Error.t
  }

exception Invalid_request of request * string

val make : Hyper.link -> request
  (* raises: 
      Url_Lexing
      Invalid_link
   *)
end



module Document : sig
  (* identification of a Web document *)
  type document_id = {
    document_url : Url.t;
      (* URL of the document *)
    document_stamp : int
      (* Always 0 in NN 3.0 *)
    }

  (* This type can't be used. Exported for typing purposes only *)
  type logger 

  (* Handles to document requested by the applet through the navigator *)
  type handle = {
    document_id : document_id;
      (* identification of the document *)
    document_referer : string option;
      (* URL of refering document, if any *)
    mutable document_status : int;
    mutable document_headers : string list;
      (* HTTP headers of document, or faked ones.
         In NN3.0, the only possible headers are Content-Type,
         Content-Length (not necessary valid), and 
         Last-Modified (not necessary valid)
       *)
    document_feed : Feed.t;
      (* where to get the data *)
    document_fragment : string option;
      (* fragment (#foo) if any *)
    mutable document_logger : logger
      (* how to log information relative to this document processing *)
    }

  type document_continuation = {
    document_process : handle -> unit;
      (* The callback invoked when the request for a document succeeds.
         The argument [handle] contains a Feed, from which one can
         read the document contents. *)
    document_finish : bool -> unit
      (* The callback invoked when the request for a document is aborted.
         Not supported in NN3.0 *)
     }

  val dclose : bool -> handle -> unit
    (* close a document handle (in particular, closes its feed).
       In MMM, first argument should be true in most cases, as it allows
       further accesses to the document.
     *)
end


(* Information to get back to the navigator *)
module Viewers : sig

type vparams = (string * string) list
type frame_targets = (string * Widget.widget) list

  type hyper_func = {
    hyper_visible : bool;
    hyper_title : string;
    hyper_func : frame_targets -> Hyper.link -> unit
    }

class  virtual context : (Document.document_id * vparams) -> object ('a)
  method base : Document.document_id
  method params : vparams
  method goto : Hyper.link -> unit
  method gotonew : Hyper.link -> unit
  method save : Hyper.link -> unit
  method virtual log : string -> unit
  method invoke : string -> Hyper.link -> unit    
  method add_nav : string * hyper_func -> unit
  method for_embed : vparams -> frame_targets -> 'a
  method in_embed : Document.document_id -> 'a
  method hyper_funs : (string * hyper_func) list
  (* pad: this is just because of some ugly bugs in camlp4o *)
  method with_target: frame_targets -> 'a
  method with_viewer_params: (string * string) list -> 'a
end

end

(* Registering an applet *)
module Applets : sig
  val register : string -> (Widget.widget -> Viewers.context -> unit) -> unit
    (* [register <applet name> <applet_callback>]
       The name should then be used as attribute value of "function" in the
       EMBED element (e.g. <EMBED SRC="foo.cmo" function="my function">)
       The applet callback is invoked as
        [f ctx nargs args] where
          ctx is the viewer context
          nargs is the a-list [name, value] of named EMBED arguments
          args is the list of anonymous EMBED arguments (_="foo")
     *)

  val get_toplevel_widget : Tk.options list -> Widget.widget
end

module Capabilities : sig
  type t 
  type right =
     FileR of string			(* read access to files *)
   | FileW of string			(* write acces to files *)
   | DocumentR of string			(* read access to URLs *)
      (* Document read access affects decoders, embedded viewers, as well as
	 the general retrieval mechanism *)
   (* The following rights are available only in MMM *)
   | HTMLDisplay
   | Internals

  val get : unit -> t
    (* Get the initial default capabilities for all applets defined in
       the bytecode file.
       MUST BE CALLED ONLY AT LOAD-TIME. Raises Not_found otherwise.
     *)

  val require : t -> right list -> bool
   (* get some specific capabilities, to avoid popping dialog boxes all
      over the place. Moreover, can make use of regexp
    *)

  exception Denied
   (* exception raised by various functions in modules Safe*, when
      access to a resource is denied
    *)

  end

module Retrieval(C: sig val capabilities: Capabilities.t end) : sig

val whoami : Url.t

val retrieve : Hyper.link -> Document.document_continuation -> unit
  (* [retrieve link conts] retrieves a document obtained by [link]
     through the navigator interface.
   *)
val get_image : Hyper.link -> (Url.t -> Tkanim.imageType -> unit) -> unit
  (* [get_image link cont] retrieves an image located at [link] and 
     applies the continuation [cont] on it. Images are cached.
   *)
end



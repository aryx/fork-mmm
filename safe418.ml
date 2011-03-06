(* Automatically generated. Do NOT edit *)
(***********************************************************************)
(*                                                                     *)
(*                    Objective Caml Applets API                       *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*          Francois Rouaix, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Trimmed down pervasives.ml *)

(* type 'a option = 'a Pervasives.option = None | Some of 'a *)

(* Exceptions *)

external raise : exn -> 'a = "%raise"

let failwith = Pervasives.failwith
let invalid_arg = Pervasives.invalid_arg

exception Exit
exception Assert_failure of (string * int * int)

(* Comparisons *)

external (=) : 'a -> 'a -> bool = "%equal"
external (<>) : 'a -> 'a -> bool = "%notequal"
external (<) : 'a -> 'a -> bool = "%lessthan"
external (>) : 'a -> 'a -> bool = "%greaterthan"
external (<=) : 'a -> 'a -> bool = "%lessequal"
external (>=) : 'a -> 'a -> bool = "%greaterequal"
external compare: 'a -> 'a -> int = "compare"

let min = Pervasives.min
let max = Pervasives.max

external (==) : 'a -> 'a -> bool = "%eq"
external (!=) : 'a -> 'a -> bool = "%noteq"

(* Boolean operations *)

external not : bool -> bool = "%boolnot"
external (&) : bool -> bool -> bool = "%sequand"
external (&&) : bool -> bool -> bool = "%sequand"
external (or) : bool -> bool -> bool = "%sequor"
external (||) : bool -> bool -> bool = "%sequor"

(* Integer operations *)

external (~-) : int -> int = "%negint"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external (+) : int -> int -> int = "%addint"
external (-) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external (/) : int -> int -> int = "%divint"
external (mod) : int -> int -> int = "%modint"

let abs = Pervasives.abs

external (land) : int -> int -> int = "%andint"
external (lor) : int -> int -> int = "%orint"
external (lxor) : int -> int -> int = "%xorint"

let lnot = Pervasives.lnot

external (lsl) : int -> int -> int = "%lslint"
external (lsr) : int -> int -> int = "%lsrint"
external (asr) : int -> int -> int = "%asrint"

let min_int = Pervasives.min_int
let max_int = Pervasives.max_int

(* Floating-point operations *)

external (~-.) : float -> float = "%negfloat"
external (+.) : float -> float -> float = "%addfloat"
external (-.) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external (/.) : float -> float -> float = "%divfloat"
external ( ** ) : float -> float -> float = "power_float" "pow" "float"
external exp : float -> float = "exp_float" "exp" "float"
external acos : float -> float = "acos_float" "acos" "float"
external asin : float -> float = "asin_float" "asin" "float"
external atan : float -> float = "atan_float" "atan" "float"
external atan2 : float -> float -> float = "atan2_float" "atan2" "float"
external cos : float -> float = "cos_float" "cos" "float"
external cosh : float -> float = "cosh_float" "cosh" "float"
external log : float -> float = "log_float" "log" "float"
external log10 : float -> float = "log10_float" "log10" "float"
external sin : float -> float = "sin_float" "sin" "float"
external sinh : float -> float = "sinh_float" "sinh" "float"
external sqrt : float -> float = "sqrt_float" "sqrt" "float"
external tan : float -> float = "tan_float" "tan" "float"
external tanh : float -> float = "tanh_float" "tanh" "float"
external ceil : float -> float = "ceil_float" "ceil" "float"
external floor : float -> float = "floor_float" "floor" "float"
external abs_float : float -> float = "%absfloat"
external mod_float : float -> float -> float = "fmod_float" "fmod" "float"
external frexp : float -> float * int = "frexp_float"
external ldexp : float -> int -> float = "ldexp_float"
external modf : float -> float * float = "modf_float"
external float : int -> float = "%floatofint"
external float_of_int : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"

(* String operations -- more in module String *)

external string_length : string -> int = "ml_string_length"
external string_create: int -> string = "create_string"
external string_blit : string -> int -> string -> int -> int -> unit
                     = "blit_string"

let (^) = Pervasives.(^)

(* Character operations -- more in module Char *)

external int_of_char : char -> int = "%identity"
let char_of_int = Pervasives.char_of_int

(* Unit operations *)

external ignore : 'a -> unit = "%ignore"

(* Pair operations *)

external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"

(* String conversion functions *)

external format_int: string -> int -> string = "format_int"
external format_float: string -> float -> string = "format_float"

let string_of_bool = Pervasives.string_of_bool
let bool_of_string = Pervasives.bool_of_string

let string_of_int = Pervasives.string_of_int

external int_of_string : string -> int = "int_of_string"

let string_of_float = Pervasives.string_of_float

external float_of_string : string -> float = "float_of_string"

(* List operations -- more in module List *)

let (@) = Pervasives.(@)

(* I/O operations *)
(* for applets, we only keep stdout and stderr output, and stdin input *)

let print_char = Pervasives.print_char
let print_string = Pervasives.print_string
let print_int = Pervasives.print_int
let print_float = Pervasives.print_float
let print_endline = Pervasives.print_endline
let print_newline = Pervasives.print_newline

(* Output functions on standard error *)

let prerr_char = Pervasives.prerr_char
let prerr_string = Pervasives.prerr_string
let prerr_int = Pervasives.prerr_int
let prerr_float = Pervasives.prerr_float
let prerr_endline = Pervasives.prerr_endline
let prerr_newline = Pervasives.prerr_newline

(* Input functions on standard input *)

let read_line = Pervasives.read_line
let read_int = Pervasives.read_int
let read_float = Pervasives.read_float

(* References *)

type 'a ref = 'a Pervasives.ref = { mutable contents: 'a }
external ref: 'a -> 'a ref = "%makemutable"
external (!): 'a ref -> 'a = "%field0"
external (:=): 'a ref -> 'a -> unit = "%setfield0"
external incr: int ref -> unit = "%incr"
external decr: int ref -> unit = "%decr"

(* Miscellaneous *)

(* sys_exit is not exported *)

(* Other modules from the standard library *)

(* Arg: not needed *)
module Array = Array
(* Callback: not needed *)
module Char = Char
module Digest = Digest 
(* Filename: probably not needed *)
(* Format: probably not needed *)
(* Gc: probably not needed *)
module Genlex = Genlex
module Hashtbl = Hashtbl
module Lexing = Lexing
module List = List
module Map = Map
(* Marshal : it is unsafe because it can now export closures *)
(* Obj: inherently unsafe *)
(* Oo: *)
(* Parsing : inherently unsafe *)
module Printexc = Printexc
module Printf = Printf
module Queue = Queue
module Random = Random
module Set = Set
module Sort = Sort
module Stack = Stack
module Stream = Stream
module Buffer = Buffer
module String = String
(* Sys: nothing useful *)
(* Weak: advanced... *)
module Widget = Widget
module Protocol = Protocol
module Textvariable = Textvariable
module Timer = Timer
module Tk = Tk
module Place = Place
module Resource = Resource
module Wm = Wm
module Imagephoto = Imagephoto
module Canvas = Canvas
module Button = Button
module Text = Text
module Label = Label
module Scrollbar = Scrollbar
module Pixmap = Pixmap
module Palette = Palette
module Message = Message
module Menu = Menu
module Entry = Entry
module Listbox = Listbox
module Focus = Focus
module Menubutton = Menubutton
module Pack = Pack
module Toplevel = Toplevel
module Frame = Frame
module Dialog = Dialog
module Imagebitmap = Imagebitmap
module Clipboard = Clipboard
module Radiobutton = Radiobutton
module Tkwait = Tkwait
module Grab = Grab
module Selection = Selection
module Scale = Scale
module Optionmenu = Optionmenu
module Winfo = Winfo
module Grid = Grid
module Checkbutton = Checkbutton
module Bell = Bell
module Frx_misc = Frx_misc
module Frx_req = Frx_req 
module Frx_dialog = Frx_dialog
module Jtk = Jtk
module Tkanim = Tkanim
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

module Capabilities = Capabilities

open Capabilities
open Document
open Hyper
open Url
open Www

(* Various copy functions to avoid nasty holes *)
(* copy a string option *)
let copy_field = function
    None -> None
  | Some s -> Some (String.copy s)

(* copy an url, possibly masking the username/password information *)
let copy_url url maskid = {
  protocol = url.protocol;
  user = (if maskid then None else copy_field url.user);
  password = (if maskid then None else copy_field url.password);
  host = copy_field url.host;
  port = url.port;
  path = copy_field url.path;
  search = copy_field url.search;
}

let copy_link link = {
  h_uri = String.copy link.h_uri;
  h_context = copy_field link.h_context;
  h_method = link.h_method;
  h_params = List.map (fun (x,y) -> String.copy x, String.copy y) link.h_params
} 

module Retrieval(C: sig val capabilities: Capabilities.t end) = struct

(* Since this is computed at initialisation time, it should be correct *)
let ask = ask C.capabilities

(* THIS MUST BE A COPY *)
(* Note: we don't reveal to the applet the user/password that was required
   to load it. The reason is that an applet written by C (and residing on
   his site), pointed to by a page on A could reveal to C the username and 
   password of B on site A. 
 *)
let whoami = copy_url C.capabilities.who true

(* Protect on the continuation, in case the initial request
   gets redirected by an accomplice server 
    e.g. http://accomplice.com/goto?someurl
     returns Location: someurl
   SECURITY NOTE: we can't let an applet handle a wr, because a wr may contain
   a username/password to some other site (Auth.check physically modifies
   the wr to add authorization information).
   SECURITY NOTE: if wr belongs to the applet world, it can be mutated between
   the emission of the request and the invocation of the continuation
   document_process. Since the Url in wr.www_url is shared with the 
   dh.document_id, we have to make our own copy or wr to be sure we check
   the propoer document_url.
  *)
let rec retrieve hlink cont = 
  let hlink = copy_link hlink in (* avoid future mutations *) 
  let wr = Www.make hlink in
  let retry hlink = retrieve hlink cont
  and real_cont = {
    document_process = (fun dh ->
      let url = Url.string_of dh.document_id.document_url in
       if ask (DocumentR url) then cont.document_process dh
       else cont.document_finish true);
    document_finish = cont.document_finish
    }
  in
  match wr.www_url.protocol with
    MAILTO -> raise Denied
  | _ ->
    if Nav.dont_check_cache wr then ignore (Retrieve.f wr retry real_cont)
    else 
      let did = {document_url = wr.www_url; document_stamp = no_stamp} in
      try
	let doc = Cache.find did in
	  real_cont.document_process (Cache.make_handle wr doc)
      with 
	Not_found -> (* we don't have the document *)
	  ignore (Retrieve.f wr retry real_cont)

(* Images are public, although they also may contain private information *)
(* JPF : progression callback is inavailable in applets *)
(*          it is just for keeping consistency *)
let get_image hlink cont =
  Img.get {document_url = C.capabilities.who; document_stamp = no_stamp}
          hlink cont Progress.no_meter

end

module Mstring = Mstring
module Mlist = Mlist
module Feed = Feed 
module Error = Error
module Hyper = Hyper
module Www = Www
module Url = Url
module Lexurl = Lexurl
module Document = Document 
module Applets = Applets 
module Viewers = Viewers
(***********************************************************************)
(*                                                                     *)
(*                           Caml Applets                              *)
(*                                                                     *)
(*          Francois Rouaix, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Capabilities

module GetIO(C: sig val capabilities: t end) = 
  struct

  let ask = ask C.capabilities
  
type in_channel = {
  in_channel : Pervasives.in_channel;
  mutable in_status : bool
  }
type out_channel = {
  out_channel : Pervasives.out_channel;
  mutable out_status : bool
  }

type open_flag = Pervasives.open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock

let denied () = raise (Sys_error (I18n.sprintf "Permission denied"))

(* Do *not* give standard channels stdin/stdout/stderr *)

let open_out name =
  let name = String.copy name in
  if ask (FileW name)
  then {out_channel = open_out name; out_status = true}
  else denied()

let open_out_bin name =
  let name = String.copy name in
  if ask (FileW name)
  then {out_channel = open_out_bin name;
      	out_status = true}
  else denied()

let open_out_gen flags mode name =
  let name = String.copy name in
  if ask (FileW name)
  then {out_channel = open_out_gen flags mode name;
      	out_status = true}
  else denied()

let flush woc = 
  if woc.out_status then flush woc.out_channel
  else invalid_arg "flush"

let output_char woc c = 
  if woc.out_status then output_char woc.out_channel c
  else invalid_arg "output_char"

let output_string woc s =
  if woc.out_status then output_string woc.out_channel s
  else invalid_arg "output_string"

let output woc buff ofs len =
  if woc.out_status then output woc.out_channel buff ofs len
  else invalid_arg "output"

let output_byte woc n = 
  if woc.out_status then output_byte woc.out_channel n
  else invalid_arg "output_byte"

let output_binary_int woc n =
  if woc.out_status then output_binary_int woc.out_channel n
  else invalid_arg "output_binary_int"

let output_value woc v =
  if woc.out_status then output_value woc.out_channel v
  else invalid_arg "output_value"

let seek_out woc n =
  if woc.out_status then seek_out woc.out_channel n
  else invalid_arg "seek_out"

let pos_out woc =
  if woc.out_status then pos_out woc.out_channel
  else invalid_arg "pos_out"

let out_channel_length woc =
  if woc.out_status then out_channel_length woc.out_channel
  else invalid_arg "out_channel_length"

let close_out woc = 
  if woc.out_status then begin
    woc.out_status <- false;
    close_out woc.out_channel
    end
  else invalid_arg "n.close_out"


(* General input functions *)

let open_in name =
  let name = String.copy name in
  if ask (FileR name)
  then {in_channel = open_in name; in_status = true}
  else denied()

let open_in_bin name =
  let name = String.copy name in
  if ask (FileR name)
  then {in_channel = open_in_bin name; in_status = true}
  else denied()

let open_in_gen flags mode name =
  let name = String.copy name in
  if ask (FileR name)
  then {in_channel = open_in_gen flags mode name; in_status = true}
  else denied()

let input_char wic =
  if wic.in_status then input_char wic.in_channel
  else invalid_arg "input_char"

let input_line wic =
  if wic.in_status then input_line wic.in_channel
  else invalid_arg "input_line"

let input wic buf ofs len =
  if wic.in_status then input wic.in_channel buf ofs len
  else invalid_arg "input"

let really_input wic buf ofs len =
  if wic.in_status then really_input wic.in_channel buf ofs len
  else invalid_arg "really_input"

let input_byte wic =
  if wic.in_status then input_byte wic.in_channel
  else invalid_arg "input_byte"

let input_binary_int wic =
  if wic.in_status then input_binary_int wic.in_channel
  else invalid_arg "input_binary_int"

(* input_value is unsafe *)
let seek_in wic n =
  if wic.in_status then seek_in wic.in_channel n
  else invalid_arg "seek_in"

let pos_in wic =
  if wic.in_status then pos_in wic.in_channel
  else invalid_arg "pos_in"

let in_channel_length wic =
  if wic.in_status then in_channel_length wic.in_channel
  else invalid_arg "in_channel_length"

let close_in wic = 
  if wic.in_status then begin
    wic.in_status <- false;
    close_in wic.in_channel
    end
  else invalid_arg "close_in"

end
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

module Str = Str
(***********************************************************************)
(*                                                                     *)
(*                           Caml Applets                              *)
(*                                                                     *)
(*          Francois Rouaix, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Capabilities
open Unix

module GetUnix(C: sig val capabilities: t end) = 
  struct

  let ask = ask C.capabilities
  
type error = Unix.error =
  (* Errors defined in the POSIX standard *)
    E2BIG               (* Argument list too long *)
  | EACCES              (* Permission denied *)
  | EAGAIN              (* Resource temporarily unavailable; try again *)
  | EBADF               (* Bad file descriptor *)
  | EBUSY               (* Resource unavailable *)
  | ECHILD              (* No child process *)
  | EDEADLK             (* Resource deadlock would occur *)
  | EDOM                (* Domain error for math functions, etc. *)
  | EEXIST              (* File exists *)
  | EFAULT              (* Bad address *)
  | EFBIG               (* File too large *)
  | EINTR               (* Function interrupted by signal *)
  | EINVAL              (* Invalid argument *)
  | EIO                 (* Hardware I/O error *)
  | EISDIR              (* Is a directory *)
  | EMFILE              (* Too many open files by the process *)
  | EMLINK              (* Too many links *)
  | ENAMETOOLONG        (* Filename too long *)
  | ENFILE              (* Too many open files in the system *)
  | ENODEV              (* No such device *)
  | ENOENT              (* No such file or directory *)
  | ENOEXEC             (* Not an executable file *)
  | ENOLCK              (* No locks available *)
  | ENOMEM              (* Not enough memory *)
  | ENOSPC              (* No space left on device *)
  | ENOSYS              (* Function not supported *)
  | ENOTDIR             (* Not a directory *)
  | ENOTEMPTY           (* Directory not empty *)
  | ENOTTY              (* Inappropriate I/O control operation *)
  | ENXIO               (* No such device or address *)
  | EPERM               (* Operation not permitted *)
  | EPIPE               (* Broken pipe *)
  | ERANGE              (* Result too large *)
  | EROFS               (* Read-only file system *)
  | ESPIPE              (* Invalid seek e.g. on a pipe *)
  | ESRCH               (* No such process *)
  | EXDEV               (* Invalid link *)
  (* Additional errors, mostly BSD *)
  | EWOULDBLOCK         (* Operation would block *)
  | EINPROGRESS         (* Operation now in progress *)
  | EALREADY            (* Operation already in progress *)
  | ENOTSOCK            (* Socket operation on non-socket *)
  | EDESTADDRREQ        (* Destination address required *)
  | EMSGSIZE            (* Message too long *)
  | EPROTOTYPE          (* Protocol wrong type for socket *)
  | ENOPROTOOPT         (* Protocol not available *)
  | EPROTONOSUPPORT     (* Protocol not supported *)
  | ESOCKTNOSUPPORT     (* Socket type not supported *)
  | EOPNOTSUPP          (* Operation not supported on socket *)
  | EPFNOSUPPORT        (* Protocol family not supported *)
  | EAFNOSUPPORT        (* Address family not supported by protocol family *)
  | EADDRINUSE          (* Address already in use *)
  | EADDRNOTAVAIL       (* Can't assign requested address *)
  | ENETDOWN            (* Network is down *)
  | ENETUNREACH         (* Network is unreachable *)
  | ENETRESET           (* Network dropped connection on reset *)
  | ECONNABORTED        (* Software caused connection abort *)
  | ECONNRESET          (* Connection reset by peer *)
  | ENOBUFS             (* No buffer space available *)
  | EISCONN             (* Socket is already connected *)
  | ENOTCONN            (* Socket is not connected *)
  | ESHUTDOWN           (* Can't send after socket shutdown *)
  | ETOOMANYREFS        (* Too many references: can't splice *)
  | ETIMEDOUT           (* Connection timed out *)
  | ECONNREFUSED        (* Connection refused *)
  | EHOSTDOWN           (* Host is down *)
  | EHOSTUNREACH        (* No route to host *)
  | ELOOP               (* Too many levels of symbolic links *)
  | EOVERFLOW
  (* All other errors are mapped to EUNKNOWNERR *)
  | EUNKNOWNERR of int  (* Unknown error *)

        (* The type of error codes. *)

exception Unix_error of error * string * string
        (* Raised by the system calls below when an error is encountered.
           The first component is the error code; the second component
           is the function name; the third component is the string parameter
           to the function, if it has one, or the empty string otherwise. *)

external error_message : error -> string = "unix_error_message"
        (* Return a string describing the given error code. *)

type file_descr = {
  file_descr : Unix.file_descr;
  mutable fd_status : bool
  }

type open_flag = Unix.open_flag =
    O_RDONLY                            (* Open for reading *)
  | O_WRONLY                            (* Open for writing *)
  | O_RDWR                              (* Open for reading and writing *)
  | O_NONBLOCK                          (* Open in non-blocking mode *)
  | O_APPEND                            (* Open for append *)
  | O_CREAT                             (* Create if nonexistent *)
  | O_TRUNC                             (* Truncate to 0 length if existing *)
  | O_EXCL                              (* Fail if existing *)

  | O_NOCTTY                    (** Don't make this dev a controlling tty *)
  | O_DSYNC                     (** Writes complete as `Synchronised I/O data integrity completion' *)
  | O_SYNC                      (** Writes complete as `Synchronised I/O file integrity completion' *)
  | O_RSYNC                     (** Reads complete as writes (depending on O_SYNC/O_DSYNC) *)

        (* The flags to [open]. *)

type file_perm = int
        (* The type of file access rights. *)

let openfile fname flags perm =
  let name = String.copy fname in
  if ask (FileR name)
  then {file_descr = openfile name flags perm; fd_status = true}
  else raise (Unix_error (EACCES, "openfile", name))

let close wfd =
  if wfd.fd_status then begin
    wfd.fd_status <- false;
    close wfd.file_descr
    end
  else
    raise (Unix_error (EBADF, "close", ""))

let read wfd buf ofs len =
  if wfd.fd_status then read wfd.file_descr buf ofs len
  else raise (Unix_error (EINVAL, "read", ""))

let write wfd buf ofs len =
  if wfd.fd_status then write wfd.file_descr buf ofs len
  else raise (Unix_error (EINVAL, "write", ""))

type seek_command = Unix.seek_command =
    SEEK_SET
  | SEEK_CUR
  | SEEK_END

        (* Positioning modes for [lseek]. [SEEK_SET] indicates positions
           relative to the beginning of the file, [SEEK_CUR] relative to
           the current position, [SEEK_END] relative to the end of the
           file. *)

let lseek wfd n cmd =
  if wfd.fd_status then lseek wfd.file_descr n cmd
  else raise (Unix_error (EINVAL, "lseek", ""))

let ftruncate wfd n =
  if wfd.fd_status then ftruncate wfd.file_descr n
  else raise (Unix_error (EINVAL, "ftruncate", ""))

(*** File statistics *)

type file_kind = Unix.file_kind =
    S_REG                               (* Regular file *)
  | S_DIR                               (* Directory *)
  | S_CHR                               (* Character device *)
  | S_BLK                               (* Block device *)
  | S_LNK                               (* Symbolic link *)
  | S_FIFO                              (* Named pipe *)
  | S_SOCK                              (* Socket *)

type stats = Unix.stats =
  { st_dev : int;                       (* Device number *)
    st_ino : int;                       (* Inode number *)
    st_kind : file_kind;                (* Kind of the file *)
    st_perm : file_perm;                (* Access rights *)
    st_nlink : int;                     (* Number of links *)
    st_uid : int;                       (* User id of the owner *)
    st_gid : int;                       (* Group id of the owner *)
    st_rdev : int;                      (* Device minor number *)
    st_size : int;                      (* Size in bytes *)
    st_atime : float;                     (* Last access time *)
    st_mtime : float;                     (* Last modification time *)
    st_ctime : float }                    (* Last status change time *)

        (* The informations returned by the [stat] calls. *)

let fstat wfd =
  if wfd.fd_status then fstat wfd.file_descr 
  else raise (Unix_error (EINVAL, "fstat", ""))

let fchmod wfd perms =
  if wfd.fd_status then fchmod wfd.file_descr perms
  else raise (Unix_error (EINVAL, "fchmod", ""))

let getcwd = getcwd

type dir_handle = {
  dir_handle : Unix.dir_handle;
  mutable dh_status : bool
  }

let opendir fname =
  let name = String.copy fname in
  if ask (FileR name)
  then {dir_handle = opendir fname; dh_status = true}
  else raise (Unix_error (EACCES, "opendir", name))

let readdir dh =
  if dh.dh_status then readdir dh.dir_handle
  else raise (Unix_error (EINVAL, "readdir", ""))

let rewinddir dh =
  if dh.dh_status then rewinddir dh.dir_handle
  else raise (Unix_error (EINVAL, "rewinddir", ""))

let closedir dh =
  if dh.dh_status then begin
      dh.dh_status <- false;
      closedir dh.dir_handle
      end
  else raise (Unix_error (EINVAL, "closedir", ""))


(*** Time functions *)

type process_times = Unix.process_times =
  { tms_utime : float;          (* User time for the process *)
    tms_stime : float;          (* System time for the process *)
    tms_cutime : float;         (* User time for the children processes *)
    tms_cstime : float }        (* System time for the children processes *)

        (* The execution times (CPU times) of a process. *)

type tm = Unix.tm =
  { tm_sec : int;                       (* Seconds 0..59 *)
    tm_min : int;                       (* Minutes 0..59 *)
    tm_hour : int;                      (* Hours 0..23 *)
    tm_mday : int;                      (* Day of month 1..31 *)
    tm_mon : int;                       (* Month of year 0..11 *)
    tm_year : int;                      (* Year - 1900 *)
    tm_wday : int;                      (* Day of week (Sunday is 0) *)
    tm_yday : int;                      (* Day of year 0..365 *)
    tm_isdst : bool }                   (* Daylight time savings in effect *)

        (* The type representing wallclock time and calendar date. *)

let time = time
and gettimeofday = gettimeofday
and gmtime = gmtime
and localtime = localtime
and mktime = mktime
and times = times
end

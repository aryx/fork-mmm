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
module GetUnix(C: sig val capabilities: Capabilities.t end) : sig
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

(*** Basic file input/output *)

type file_descr
        (* The abstract type of file descriptors. *)

type open_flag = 
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
  | O_SHARE_DELETE
  | O_CLOEXEC                   (** Set the close-on-exec flag on the
                                   descriptor returned by {!openfile} *)
  (* NEW *)
  | O_KEEPEXEC

        (* The flags to [open]. *)

type file_perm = int
        (* The type of file access rights. *)

val openfile : string -> open_flag list -> file_perm -> file_descr
        (* Open the named file with the given flags. Third argument is
           the permissions to give to the file if it is created. Return
           a file descriptor on the named file. *)
val close : file_descr -> unit
        (* Close a file descriptor. *)
val read : file_descr -> string -> int -> int -> int
        (* [read fd buff ofs len] reads [len] characters from descriptor
           [fd], storing them in string [buff], starting at position [ofs]
           in string [buff]. Return the number of characters actually read. *)
val write : file_descr -> string -> int -> int -> int
        (* [write fd buff ofs len] writes [len] characters to descriptor
           [fd], taking them from string [buff], starting at position [ofs]
           in string [buff]. Return the number of characters actually
           written. *)

(*** Seeking and truncating *)

type seek_command = 
    SEEK_SET
  | SEEK_CUR
  | SEEK_END

        (* Positioning modes for [lseek]. [SEEK_SET] indicates positions
           relative to the beginning of the file, [SEEK_CUR] relative to
           the current position, [SEEK_END] relative to the end of the
           file. *)

val lseek : file_descr -> int -> seek_command -> int
        (* Set the current position for a file descriptor *)

val ftruncate : file_descr -> int -> unit
        (* Truncates the file corresponding to the given descriptor
           to the given size. *)
(*** File statistics *)

type file_kind = 
    S_REG                               (* Regular file *)
  | S_DIR                               (* Directory *)
  | S_CHR                               (* Character device *)
  | S_BLK                               (* Block device *)
  | S_LNK                               (* Symbolic link *)
  | S_FIFO                              (* Named pipe *)
  | S_SOCK                              (* Socket *)

type stats = 
  { st_dev : int;                       (* Device number *)
    st_ino : int;                       (* Inode number *)
    st_kind : file_kind;                (* Kind of the file *)
    st_perm : file_perm;                (* Access rights *)
    st_nlink : int;                     (* Number of links *)
    st_uid : int;                       (* User id of the owner *)
    st_gid : int;                       (* Group id of the owner *)
    st_rdev : int;                      (* Device minor number *)
    st_size : int;                      (* Size in bytes *)
    st_atime : float;                   (* Last access time *)
    st_mtime : float;                   (* Last modification time *)
    st_ctime : float }                  (* Last status change time *)

        (* The informations returned by the [stat] calls. *)

val fstat : file_descr -> stats
        (* Return the information for the file associated with the given
           descriptor. *)

val fchmod : file_descr -> file_perm -> unit
        (* Change the permissions of an opened file. *) 

val getcwd : unit -> string
        (* Return the name of the current working directory. *)

type dir_handle

        (* The type of descriptors over opened directories. *)

val opendir : string -> dir_handle
        (* Open a descriptor on a directory *)
val readdir : dir_handle -> string
        (* Return the next entry in a directory.
           Raise [End_of_file] when the end of the directory has been 
           reached. *)
val rewinddir : dir_handle -> unit
        (* Reposition the descriptor to the beginning of the directory *)
val closedir : dir_handle -> unit
        (* Close a directory descriptor. *)

(*** Time functions *)

type process_times = 
  { tms_utime : float;          (* User time for the process *)
    tms_stime : float;          (* System time for the process *)
    tms_cutime : float;         (* User time for the children processes *)
    tms_cstime : float }        (* System time for the children processes *)

        (* The execution times (CPU times) of a process. *)

type tm = 
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

val time : unit -> float
        (* Return the current time since 00:00:00 GMT, Jan. 1, 1970,
           in seconds. *)
val gettimeofday : unit -> float
        (* Same as [time], but with resolution better than 1 second. *)
val gmtime : float -> tm
        (* Convert a time in seconds, as returned by [time], into a date and
           a time. Assumes Greenwich meridian time zone. *)
val localtime : float -> tm
        (* Convert a time in seconds, as returned by [time], into a date and
           a time. Assumes the local time zone. *)
val times : unit -> process_times
        (* Return the execution times of the process. *)
val mktime : tm -> float * tm
        (* Convert a date and time, specified by the [tm] argument, into
           a time in seconds, as returned by [time]. Also return a normalized
           copy of the given [tm] record, with the [tm_wday] and [tm_yday]
           recomputed from the other fields. *)
end

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
  | O_SHARE_DELETE
  | O_CLOEXEC                   (** Set the close-on-exec flag on the
                                   descriptor returned by {!openfile} *)
  (* NEW *)
  | O_KEEPEXEC

        (* The flags to [open]. *)

type file_perm = int
        (* The type of file access rights. *)

let openfile fname flags perm =
  if ask (FileR fname)
  then {file_descr = openfile fname flags perm; fd_status = true}
  else raise (Unix_error (EACCES, "openfile", fname))

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
  if ask (FileR fname)
  then {dir_handle = opendir fname; dh_status = true}
  else raise (Unix_error (EACCES, "opendir", fname))

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

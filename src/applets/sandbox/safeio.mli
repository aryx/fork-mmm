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

module GetIO(C: sig val capabilities: Capabilities.t end) : sig
type in_channel
type out_channel
        (* The types of input channels and output channels. *)

(** General output functions *)

type open_flag =
    Open_rdonly | Open_wronly | Open_append
  | Open_creat | Open_trunc | Open_excl
  | Open_binary | Open_text | Open_nonblock
        (* Opening modes for [open_out_gen] and [open_in_gen].
-          [Open_rdonly]: open for reading.
-          [Open_wronly]: open for writing.
-          [Open_append]: open for appending.
-          [Open_creat]: create the file if it does not exist.
-          [Open_trunc]: empty the file if it already exists.
-          [Open_excl]: fail if the file already exists.
-          [Open_binary]: open in binary mode (no conversion).
-          [Open_text]: open in text mode (may perform conversions).
-          [Open_nonblock]: open in non-blocking mode. *)
           
val open_out : string -> out_channel
        (* Open the named file for writing, and return a new output channel
           on that file, positionned at the beginning of the file. The
           file is truncated to zero length if it already exists. It
           is created if it does not already exists.
           Raise [Sys_error] if the file could not be opened. *)
val open_out_bin : string -> out_channel
        (* Same as [open_out], but the file is opened in binary mode,
           so that no translation takes place during writes. On operating
           systems that do not distinguish between text mode and binary
           mode, this function behaves like [open_out]. *)
val open_out_gen : open_flag list -> int -> string -> out_channel
        (* [open_out_gen mode rights filename] opens the file named
           [filename] for writing, as above. The extra argument [mode]
           specify the opening mode. The extra argument [rights] specifies
           the file permissions, in case the file must be created.
           [open_out] and [open_out_bin] are special cases of this function. *)
val flush : out_channel -> unit
        (* Flush the buffer associated with the given output channel, 
           performing all pending writes on that channel.
           Interactive programs must be careful about flushing standard
           output and standard error at the right time. *)
val output_char : out_channel -> char -> unit
        (* Write the character on the given output channel. *)
val output_string : out_channel -> string -> unit
        (* Write the string on the given output channel. *)
val output : out_channel -> bytes -> int -> int -> unit
        (* [output chan buff ofs len] writes [len] characters from string 
           [buff], starting at offset [ofs], to the output channel [chan].
           Raise [Invalid_argument "output"] if [ofs] and [len] do not
           designate a valid substring of [buff]. *)
val output_byte : out_channel -> int -> unit
        (* Write one 8-bit integer (as the single character with that code)
           on the given output channel. The given integer is taken modulo
           256. *)
val output_binary_int : out_channel -> int -> unit
        (* Write one integer in binary format on the given output channel.
           The only reliable way to read it back is through the
           [input_binary_int] function. The format is compatible across
           all machines for a given version of Objective Caml. *)
val seek_out : out_channel -> int -> unit
        (* [seek_out chan pos] sets the current writing position to [pos]
           for channel [chan]. This works only for regular files. On
           files of other kinds (such as terminals, pipes and sockets),
           the behavior is unspecified. *)
val pos_out : out_channel -> int
        (* Return the current writing position for the given channel. *)
val out_channel_length : out_channel -> int
        (* Return the total length (number of characters) of the
           given channel.  This works only for regular files. On files of
           other kinds, the result is meaningless. *)
val close_out : out_channel -> unit
        (* Close the given channel, flushing all buffered write operations.
           The behavior is unspecified if any of the functions above is
           called on a closed channel. *)

(** General input functions *)

val open_in : string -> in_channel
        (* Open the named file for reading, and return a new input channel
           on that file, positionned at the beginning of the file.
           Raise [Sys_error] if the file could not be opened. *)
val open_in_bin : string -> in_channel
        (* Same as [open_in], but the file is opened in binary mode,
           so that no translation takes place during reads. On operating
           systems that do not distinguish between text mode and binary
           mode, this function behaves like [open_in]. *)
val open_in_gen : open_flag list -> int -> string -> in_channel
        (* [open_in_gen mode rights filename] opens the file named
           [filename] for reading, as above. The extra arguments
           [mode] and [rights] specify the opening mode and file permissions.
           [open_in] and [open_in_bin] are special cases of this function. *)
val input_char : in_channel -> char
        (* Read one character from the given input channel.
           Raise [End_of_file] if there are no more characters to read. *)
val input_line : in_channel -> string
        (* Read characters from the given input channel, until a
           newline character is encountered. Return the string of
           all characters read, without the newline character at the end.
           Raise [End_of_file] if the end of the file is reached
           at the beginning of line. *)
val input : in_channel -> bytes -> int -> int -> int
        (* [input chan buff ofs len] attempts to read [len] characters
           from channel [chan], storing them in string [buff], starting at
           character number [ofs]. It returns the actual number of characters
           read, between 0 and [len] (inclusive).
           A return value of 0 means that the end of file was reached.
           A return value between 0 and [len] exclusive means that
           no more characters were available at that time; [input] must be
           called again to read the remaining characters, if desired.
           Exception [Invalid_argument "input"] is raised if [ofs] and [len]
           do not designate a valid substring of [buff]. *)          
val really_input : in_channel -> bytes -> int -> int -> unit
        (* [really_input chan buff ofs len] reads [len] characters
           from channel [chan], storing them in string [buff], starting at
           character number [ofs]. Raise [End_of_file] if
           the end of file is reached before [len] characters have been read.
           Raise [Invalid_argument "really_input"] if
           [ofs] and [len] do not designate a valid substring of [buff]. *)
val input_byte : in_channel -> int
        (* Same as [input_char], but return the 8-bit integer representing
           the character.
           Raise [End_of_file] if an end of file was reached. *)
val input_binary_int : in_channel -> int
        (* Read an integer encoded in binary format from the given input
           channel. See [output_binary_int].
           Raise [End_of_file] if an end of file was reached while reading the
           integer. *)
val seek_in : in_channel -> int -> unit
        (* [seek_in chan pos] sets the current reading position to [pos]
           for channel [chan]. This works only for regular files. On
           files of other kinds, the behavior is unspecified. *)
val pos_in : in_channel -> int
        (* Return the current reading position for the given channel. *)
val in_channel_length : in_channel -> int
        (* Return the total length (number of characters) of the
           given channel. This works only for regular files. On files of
           other kinds, the result is meaningless. *)
val close_in : in_channel -> unit
        (* Close the given channel. Anything can happen if any of the
           functions above is called on a closed channel. *)
end

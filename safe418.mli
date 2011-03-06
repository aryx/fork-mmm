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

(* Module [Pervasives]: the initially opened module *)

(* This module provides the built-in types (numbers, booleans,
   strings, exceptions, references, lists, arrays, ...)
   and the basic operations over these types.

   Applets must be compiled with -nopervasives, and must explicitly open
   the Safe module.
*)

(*** Predefined types *)

(*- type int *)
        (* The type of integer numbers. *)
(*- type char *)
        (* The type of characters. *)
(*- type string *)
        (* The type of character strings. *)
(*- type float *)
        (* The type of floating-point numbers. *)
(*- type bool *)
        (* The type of booleans (truth values). *)
(*- type unit = () *)
        (* The type of the unit value. *)
(*- type exn *)
        (* The type of exception values. *)
(*- type 'a array *)
        (* The type of arrays whose elements have type ['a]. *)
(*- type 'a list = [] | :: of 'a * 'a list *)
        (* The type of lists whose elements have type ['a]. *)
(*- type 'a option = None | Some of 'a *)
        (* The type of optional values. *)
(*- type ('a, 'b, 'c) format *)
        (* The type of format strings. ['a] is the type of the parameters
           of the format, ['c] is the result type for the [printf]-style
           function, and ['b] is the type of the first argument given to
           [%a] and [%t] printing functions (see module [Printf]). *)

(*** Exceptions *)

external raise : exn -> 'a = "%raise"
        (* Raise the given exception value *)
(*- exception Match_failure of (string * int * int) *)
        (* Exception raised when none of the cases of a pattern-matching
           apply. The arguments are the location of the pattern-matching
           in the source code (file name, position of first character,
           position of last character). *)
exception Assert_failure of (string * int * int)
        (* Exception raised when an assertion fails.  The arguments are
           the location of the pattern-matching in the source code
           (file name, position of first character, position of last
           character). *)
(*- exception Invalid_argument of string *)
        (* Exception raised by library functions to signal that the given
           arguments do not make sense. *)
(*- exception Failure of string *)
        (* Exception raised by library functions to signal that they are
           undefined on the given arguments. *)
(*- exception Not_found *)
        (* Exception raised by search functions when the desired object
           could not be found. *)
(*- exception Out_of_memory *)
        (* Exception raised by the garbage collector
           when there is insufficient memory to complete the computation. *)
(*- exception Stack_overflow *)
        (* Exception raised by the bytecode interpreter when the evaluation
           stack reaches its maximal size. This often indicates infinite
           or excessively deep recursion in the user's program. *)
(*- exception Sys_error of string *)
        (* Exception raised by the input/output functions to report
           an operating system error. *)
(*- exception End_of_file *)
        (* Exception raised by input functions to signal that the
           end of file has been reached. *)
(*- exception Division_by_zero *)
        (* Exception raised by division and remainder operations
           when their second argument is null. *)
exception Exit
        (* This exception is not raised by any library function.  It is
           provided for use in your programs. *)
(*- exception Sys_blocked_io *)
        (* A special case of [Sys_error] raised when no I/O is possible
           on a non-blocking I/O channel. *)

val invalid_arg : string -> 'a
        (* Raise exception [Invalid_argument] with the given string. *)
val failwith : string -> 'a
        (* Raise exception [Failure] with the given string. *)

(*** Comparisons *)

external ( = ) : 'a -> 'a -> bool = "%equal"
        (* [e1 = e2] tests for structural equality of [e1] and [e2].
           Mutable structures (e.g. references and arrays) are equal
           if and only if their current contents are structurally equal,
           even if the two mutable objects are not the same physical object.
           Equality between functional values raises [Invalid_argument].
           Equality between cyclic data structures may not terminate. *)
external ( <> ) : 'a -> 'a -> bool = "%notequal"
        (* Negation of [(=)]. *)
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
        (* Structural ordering functions. These functions coincide with
           the usual orderings over integers, characters, strings
           and floating-point numbers, and extend them to a
           total ordering over all types.
           The ordering is compatible with [(=)]. As in the case
           of [(=)], mutable structures are compared by contents.
           Comparison between functional values raises [Invalid_argument].
           Comparison between cyclic structures may not terminate. *)
external compare : 'a -> 'a -> int = "compare"
        (* [compare x y] returns [0] if [x=y], a negative integer if
           [x<y], and a positive integer if [x>y]. The same restrictions
           as for [=] apply. [compare] can be used as the comparison function
           required by the [Set] and [Map] modules. *)
val min : 'a -> 'a -> 'a
        (* Return the smaller of the two arguments. *)
val max : 'a -> 'a -> 'a
        (* Return the greater of the two arguments. *)
external ( == ) : 'a -> 'a -> bool = "%eq"
        (* [e1 == e2] tests for physical equality of [e1] and [e2].
           On integers and characters, it is the same as structural
           equality. On mutable structures, [e1 == e2] is true if and only if
           physical modification of [e1] also affects [e2].
           On non-mutable structures, the behavior of [(==)] is
           implementation-dependent, except that [e1 == e2] implies
           [e1 = e2]. *)
external ( != ) : 'a -> 'a -> bool = "%noteq"
        (* Negation of [(==)]. *)

(*** Boolean operations *)

external not : bool -> bool = "%boolnot"
        (* The boolean negation. *)
external ( && ) : bool -> bool -> bool = "%sequand"
external ( & ) : bool -> bool -> bool = "%sequand"
        (* The boolean ``and''. Evaluation is sequential, left-to-right:
           in [e1 && e2], [e1] is evaluated first, and if it returns [false],
           [e2] is not evaluated at all. *)
external ( || ) : bool -> bool -> bool = "%sequor"
external ( or ) : bool -> bool -> bool = "%sequor"
        (* The boolean ``or''. Evaluation is sequential, left-to-right:
           in [e1 || e2], [e1] is evaluated first, and if it returns [true],
           [e2] is not evaluated at all. *)

(*** Integer arithmetic *)

(* Integers are 31 bits wide (or 63 bits on 64-bit processors).
   All operations are taken modulo $2^{31}$ (or $2^{63}$).
   They do not fail on overflow. *)

external ( ~- ) : int -> int = "%negint"
        (* Unary negation. You can also write [-e] instead of [~-e]. *)
external succ : int -> int = "%succint"
        (* [succ x] is [x+1]. *)
external pred : int -> int = "%predint"
        (* [pred x] is [x-1]. *)
external ( + ) : int -> int -> int = "%addint"
        (* Integer addition. *)
external ( - ) : int -> int -> int = "%subint"
        (* Integer subtraction. *)
external (  * ) : int -> int -> int = "%mulint"
        (* Integer multiplication. *)
external ( / ) : int -> int -> int = "%divint"
        (* Integer division.
           Raise [Division_by_zero] if the second argument is 0. *)
external (mod) : int -> int -> int = "%modint"
        (* Integer remainder.  If [x >= 0] and [y > 0], the result
           of [x mod y] satisfies the following properties:
           [0 <= x mod y < y] and
           [x = (x / y) * y + x mod y].
           If [y = 0], [x mod y] raises [Division_by_zero].
           If [x < 0] or [y < 0], the result of [x mod y] is
           not specified and depends on the platform. *)
val abs : int -> int
        (* Return the absolute value of the argument. *)
val max_int : int
val min_int : int
        (* The greatest and smallest representable integers. *)


(** Bitwise operations *)

external (land) : int -> int -> int = "%andint"
        (* Bitwise logical and. *)
external (lor) : int -> int -> int = "%orint"
        (* Bitwise logical or. *)
external (lxor) : int -> int -> int = "%xorint"
        (* Bitwise logical exclusive or. *)
val lnot : int -> int
        (* Bitwise logical negation. *)
external (lsl) : int -> int -> int = "%lslint"
        (* [n lsl m] shifts [n] to the left by [m] bits. *)
external (lsr) : int -> int -> int = "%lsrint"
        (* [n lsr m] shifts [n] to the right by [m] bits.
           This is a logical shift: zeroes are inserted regardless of
           the sign of [n].*)
external (asr) : int -> int -> int = "%asrint"
        (* [n asr m] shifts [n] to the right by [m] bits.
           This is an arithmetic shift: the sign bit of [n] is replicated. *)

(*** Floating-point arithmetic *)

(* On most platforms, Caml's floating-point numbers follow the
   IEEE 754 standard, using double precision (64 bits) numbers.
   Floating-point operations do not fail on overflow or underflow,
   but return denormal numbers. *)

external ( ~-. ) : float -> float = "%negfloat"
        (* Unary negation. You can also write [-.e] instead of [~-.e]. *)
external ( +. ) : float -> float -> float = "%addfloat"
        (* Floating-point addition *)
external ( -. ) : float -> float -> float = "%subfloat"
        (* Floating-point subtraction *)
external ( *. ) : float -> float -> float = "%mulfloat"
        (* Floating-point multiplication *)
external ( /. ) : float -> float -> float = "%divfloat"
        (* Floating-point division. *)
external ( ** ) : float -> float -> float = "power_float" "pow" "float"
        (* Exponentiation *)
external sqrt : float -> float = "sqrt_float" "sqrt" "float"
        (* Square root *)
external exp : float -> float = "exp_float" "exp" "float"
external log : float -> float = "log_float" "log" "float"
external log10 : float -> float = "log10_float" "log10" "float"
        (* Exponential, natural logarithm, base 10 logarithm. *)
external cos : float -> float = "cos_float" "cos" "float"
external sin : float -> float = "sin_float" "sin" "float"
external tan : float -> float = "tan_float" "tan" "float"
external acos : float -> float = "acos_float" "acos" "float"
external asin : float -> float = "asin_float" "asin" "float"
external atan : float -> float = "atan_float" "atan" "float"
external atan2 : float -> float -> float = "atan2_float" "atan2" "float"
        (* The usual trigonometric functions *)
external cosh : float -> float = "cosh_float" "cosh" "float"
external sinh : float -> float = "sinh_float" "sinh" "float"
external tanh : float -> float = "tanh_float" "tanh" "float"
        (* The usual hyperbolic trigonometric functions *)
external ceil : float -> float = "ceil_float" "ceil" "float"
external floor : float -> float = "floor_float" "floor" "float"
        (* Round the given float to an integer value.
           [floor f] returns the greatest integer value less than or
           equal to [f].
           [ceil f] returns the least integer value greater than or
           equal to [f]. *)
external abs_float : float -> float = "%absfloat"
        (* Return the absolute value of the argument. *)
external mod_float : float -> float -> float = "fmod_float" "fmod" "float"
        (* [mod_float a b] returns the remainder of [a] with respect to
           [b].  The returned value is [a -. n *. b], where [n]
           is the quotient [a /. b] rounded towards zero to an integer. *)
external frexp : float -> float * int = "frexp_float"
        (* [frexp f] returns the pair of the significant
           and the exponent of [f].  When [f] is zero, the
           significant [x] and the exponent [n] of [f] are equal to
           zero.  When [f] is non-zero, they are defined by
           [f = x *. 2 ** n] and [0.5 <= x < 1.0]. *)
external ldexp : float -> int -> float = "ldexp_float"
        (* [ldexp x n] returns [x *. 2 ** n]. *)
external modf : float -> float * float = "modf_float"
        (* [modf f] returns the pair of the fractional and integral
           part of [f]. *)
external float : int -> float = "%floatofint"
external float_of_int : int -> float = "%floatofint"
        (* Convert an integer to floating-point. *)
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"
        (* Truncate the given floating-point number to an integer.
           The result is unspecified if it falls outside the
           range of representable integers. *)

(*** String operations *)

(* More string operations are provided in module [String]. *)

val ( ^ ) : string -> string -> string
        (* String concatenation. *)

(*** Character operations *)

(* More character operations are provided in module [Char]. *)

external int_of_char : char -> int = "%identity"
        (* Return the ASCII code of the argument. *)
val char_of_int : int -> char
        (* Return the character with the given ASCII code.
           Raise [Invalid_argument "char_of_int"] if the argument is
           outside the range 0--255. *)

(*** Unit operations *)

external ignore : 'a -> unit = "%ignore"
        (* Discard the value of its argument and return [()].
           For instance, [ignore(f x)] discards the result of
           the side-effecting function [f].  It is equivalent to
           [f x; ()], except that only a partial application warning
           may be generated. *)

(*** String conversion functions *)

val string_of_bool : bool -> string
        (* Return the string representation of a boolean. *)
val bool_of_string : string -> bool
        (* Convert the given string to a boolean.
           Raise [Invalid_argument "bool_of_string"] if the string is not
           ["true"] or ["false"]. *)
val string_of_int : int -> string
        (* Return the string representation of an integer, in decimal. *)
external int_of_string : string -> int = "int_of_string"
        (* Convert the given string to an integer.
           The string is read in decimal (by default) or in hexadecimal,
           octal or binary if the string begins with [0x], [0o] or [0b]
           respectively.
           Raise [Failure "int_of_string"] if the given string is not
           a valid representation of an integer. *)
val string_of_float : float -> string
        (* Return the string representation of a floating-point number. *)
external float_of_string : string -> float = "float_of_string"
        (* Convert the given string to a float.
           The result is unspecified if the given string is not
           a valid representation of a float. *)

(*** Pair operations *)

external fst : 'a * 'b -> 'a = "%field0"
        (* Return the first component of a pair. *)
external snd : 'a * 'b -> 'b = "%field1"
        (* Return the second component of a pair. *)

(*** List operations *)

(* More list operations are provided in module [List]. *)

val ( @ ) : 'a list -> 'a list -> 'a list
        (* List concatenation. *)

(*** Input/output *)

(* UNSAFE type in_channel *)
(* UNSAFE type out_channel
        (* The types of input channels and output channels. *) *)

(* Output functions on standard output *)

val print_char : char -> unit
        (* Print a character on standard output. *)
val print_string : string -> unit
        (* Print a string on standard output. *)
val print_int : int -> unit
        (* Print an integer, in decimal, on standard output. *)
val print_float : float -> unit
        (* Print a floating-point number, in decimal, on standard output. *)
val print_endline : string -> unit
        (* Print a string, followed by a newline character, on
           standard output. *)
val print_newline : unit -> unit
        (* Print a newline character on standard output, and flush
           standard output. This can be used to simulate line
           buffering of standard output. *)

(** Output functions on standard error *)

val prerr_char : char -> unit
        (* Print a character on standard error. *)
val prerr_string : string -> unit
        (* Print a string on standard error. *)
val prerr_int : int -> unit
        (* Print an integer, in decimal, on standard error. *)
val prerr_float : float -> unit
        (* Print a floating-point number, in decimal, on standard error. *)
val prerr_endline : string -> unit
        (* Print a string, followed by a newline character on standard error
           and flush standard error. *)
val prerr_newline : unit -> unit
        (* Print a newline character on standard error, and flush
           standard error. *)

(** Input functions on standard input *)

val read_line : unit -> string
        (* Flush standard output, then read characters from standard input
           until a newline character is encountered. Return the string of
           all characters read, without the newline character at the end. *)
val read_int : unit -> int
        (* Flush standard output, then read one line from standard input
           and convert it to an integer. Raise [Failure "int_of_string"]
           if the line read is not a valid representation of an integer. *)
val read_float : unit -> float
        (* Flush standard output, then read one line from standard input
           and convert it to a floating-point number.
           The result is unspecified if the line read is not a valid
           representation of a floating-point number. *)

(*** References *)

type 'a ref = { mutable contents : 'a }
        (* The type of references (mutable indirection cells) containing
           a value of type ['a]. *)
external ref : 'a -> 'a ref = "%makemutable"
        (* Return a fresh reference containing the given value. *)
external ( ! ) : 'a ref -> 'a = "%field0"
        (* [!r] returns the current contents of reference [r].
           Equivalent to [fun r -> r.contents]. *)
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
        (* [r := a] stores the value of [a] in reference [r].
           Equivalent to [fun r v -> r.contents <- v]. *)
external incr : int ref -> unit = "%incr"
        (* Increment the integer contained in the given reference.
           Equivalent to [fun r -> r := succ !r]. *)
external decr : int ref -> unit = "%decr"
        (* Decrement the integer contained in the given reference.
           Equivalent to [fun r -> r := pred !r]. *)

(* Other modules from the standard library, with restrictions *)

module Array : sig
(* Module [Array]: array operations *)

external length : 'a array -> int = "%array_length"
        (* Return the length (number of elements) of the given array. *)
external get : 'a array -> int -> 'a = "%array_safe_get"
        (* [Array.get a n] returns the element number [n] of array [a].
           The first element has number 0.
           The last element has number [Array.length a - 1].
           Raise [Invalid_argument "Array.get"]  if [n] is outside the range
           0 to [(Array.length a - 1)].
           You can also write [a.(n)] instead of [Array.get a n]. *)
external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
        (* [Array.set a n x] modifies array [a] in place, replacing
           element number [n] with [x].
           Raise [Invalid_argument "Array.set"] if [n] is outside the range
           0 to [Array.length a - 1].
           You can also write [a.(n) <- x] instead of [Array.set a n x]. *)
external make : int -> 'a -> 'a array = "caml_make_vect"
external create : int -> 'a -> 'a array = "caml_make_vect"
        (* [Array.make n x] returns a fresh array of length [n],
           initialized with [x].
           All the elements of this new array are initially
           physically equal to [x] (in the sense of the [==] predicate).
           Consequently, if [x] is mutable, it is shared among all elements
           of the array, and modifying [x] through one of the array entries
           will modify all other entries at the same time.
           Raise [Invalid_argument] if [n <= 0] or [n > Sys.max_array_length].
           If the value of [x] is a floating-point number, then the maximum
           size is only [Sys.max_array_length / 2].
           [Array.create] is a deprecated alias for [Array.make]. *)
val init : int -> (int -> 'a) -> 'a array
        (* [Array.init n f] returns a fresh array of length [n],
           with element number [i] initialized to the result of [f i].
           In other terms, [Array.init n f] tabulates the results of [f]
           applied to the integers [0] to [n-1]. *)
val make_matrix : int -> int -> 'a -> 'a array array
val create_matrix : int -> int -> 'a -> 'a array array
        (* [Array.make_matrix dimx dimy e] returns a two-dimensional array
           (an array of arrays) with first dimension [dimx] and
           second dimension [dimy]. All the elements of this new matrix
           are initially physically equal to [e].
           The element ([x,y]) of a matrix [m] is accessed
           with the notation [m.(x).(y)].
           Raise [Invalid_argument] if [dimx] or [dimy] is less than 1 or
           greater than [Sys.max_array_length].
           If the value of [e] is a floating-point number, then the maximum
           size is only [Sys.max_array_length / 2].
           [Array.create_matrix] is a deprecated alias for [Array.make_matrix].
           *)
val append : 'a array -> 'a array -> 'a array
        (* [Array.append v1 v2] returns a fresh array containing the
           concatenation of the arrays [v1] and [v2]. *)
val concat : 'a array list -> 'a array
        (* Same as [Array.append], but catenates a list of arrays. *)
val sub : 'a array -> int -> int -> 'a array
        (* [Array.sub a start len] returns a fresh array of length [len],
           containing the elements number [start] to [start + len - 1]
           of array [a].
           Raise [Invalid_argument "Array.sub"] if [start] and [len] do not
           designate a valid subarray of [a]; that is, if
           [start < 0], or [len < 0], or [start + len > Array.length a]. *)
val copy : 'a array -> 'a array
        (* [Array.copy a] returns a copy of [a], that is, a fresh array
           containing the same elements as [a]. *)
val fill : 'a array -> int -> int -> 'a -> unit
        (* [Array.fill a ofs len x] modifies the array [a] in place,
           storing [x] in elements number [ofs] to [ofs + len - 1].
           Raise [Invalid_argument "Array.fill"] if [ofs] and [len] do not
           designate a valid subarray of [a]. *)
val blit : 'a array -> int ->
          'a array -> int -> int -> unit
        (* [Array.blit v1 o1 v2 o2 len] copies [len] elements
           from array [v1], starting at element number [o1], to array [v2],
           starting at element number [o2]. It works correctly even if
           [v1] and [v2] are the same array, and the source and
           destination chunks overlap.
           Raise [Invalid_argument "Array.blit"] if [o1] and [len] do not
           designate a valid subarray of [v1], or if [o2] and [len] do not
           designate a valid subarray of [v2]. *)
val to_list : 'a array -> 'a list
        (* [Array.to_list a] returns the list of all the elements of [a]. *)
val of_list : 'a list -> 'a array
        (* [Array.of_list l] returns a fresh array containing the elements
           of [l]. *)
val iter : ('a -> unit) -> 'a array -> unit
        (* [Array.iter f a] applies function [f] in turn to all
           the elements of [a].  It is equivalent to
           [f a.(0); f a.(1); ...; f a.(Array.length a - 1); ()]. *)
val map : ('a -> 'b) -> 'a array -> 'b array
        (* [Array.map f a] applies function [f] to all the elements of [a],
           and builds an array with the results returned by [f]:
           [[| f a.(0); f a.(1); ...; f a.(Array.length a - 1) |]]. *)
val iteri : (int -> 'a -> unit) -> 'a array -> unit
val mapi : (int -> 'a -> 'b) -> 'a array -> 'b array
        (* Same as [Array.iter] and [Array.map] respectively, but the
           function is applied to the index of the element as first argument,
           and the element itself as second argument. *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
        (* [Array.fold_left f x a] computes
           [f (... (f (f x a.(0)) a.(1)) ...) a.(n-1)],
           where [n] is the length of the array [a]. *)
val fold_right : ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a
        (* [Array.fold_right f a x] computes
           [f a.(0) (f a.(1) ( ... (f a.(n-1) x) ...))],
           where [n] is the length of the array [a]. *)

(** Sorting *)
val sort : ('a -> 'a -> int) -> 'a array -> unit;;
        (* Sort an array in increasing order according to a comparison
           function.  The comparison function must return 0 if it arguments
           compare as equal, a positive integer if the first is greater,
           and a negative integer if the first is smaller.  For example,
           the [compare] function is a suitable comparison function.
           After calling [Array.sort], the array is sorted in place in
           increasing order.
           [Array.sort] is guaranteed to run in constant heap space
           and logarithmic stack space.

           The current implementation uses Heap Sort.  It runs in constant
           stack space.
        *)

val stable_sort : ('a -> 'a -> int) -> 'a array -> unit;;
        (* Same as [Array.sort], but the sorting algorithm is stable and
           not guaranteed to use a fixed amount of heap memory.
           The current implementation is Merge Sort. It uses [n/2]
           words of heap space, where [n] is the length of the array.
           It is faster than the current implementation of [Array.sort].
        *)
end

module Char : sig
(* Module [Char]: character operations *)

external code : char -> int = "%identity"
        (* Return the ASCII code of the argument. *)
val chr : int -> char
        (* Return the character with the given ASCII code.
           Raise [Invalid_argument "Char.chr"] if the argument is
           outside the range 0--255. *)
val escaped : char -> string
        (* Return a string representing the given character,
           with special characters escaped following the lexical conventions
           of Objective Caml. *)
val lowercase : char -> char
val uppercase : char -> char
        (* Convert the given character to its equivalent lowercase or
           uppercase character, respectively. *)
end

module Digest : sig
(* Module [Digest]: MD5 message digest *)

(* This module provides functions to compute 128-bit ``digests'' of
   arbitrary-length strings or files. The digests are of cryptographic
   quality: it is very hard, given a digest, to forge a string having
   that digest. The algorithm used is MD5. *)

type t = string
        (* The type of digests: 16-character strings. *)
val string : string -> t
        (* Return the digest of the given string. *)
val substring : string -> int -> int -> t
        (* [Digest.substring s ofs len] returns the digest of the substring
           of [s] starting at character number [ofs] and containing [len]
           characters. *)
val file : string -> t
        (* Return the digest of the file whose name is given. *)
end

module Hashtbl : sig
(* Module [Hashtbl]: hash tables and hash functions *)

(* Hash tables are hashed association tables, with in-place modification. *)

(*** Generic interface *)

type ('a, 'b) t
        (* The type of hash tables from type ['a] to type ['b]. *)

val create : int -> ('a,'b) t
        (* [Hashtbl.create n] creates a new, empty hash table, with
           initial size [n].  For best results, [n] should be on the
           order of the expected number of elements that will be in
           the table.  The table grows as needed, so [n] is just an
           initial guess. *)

val clear : ('a, 'b) t -> unit
        (* Empty a hash table. *)

val copy : ('a, 'b) t -> ('a, 'b) t
        (* Return a copy of the given hashtable. *)

val add : ('a, 'b) t -> 'a -> 'b -> unit
        (* [Hashtbl.add tbl x y] adds a binding of [x] to [y] in table [tbl].
           Previous bindings for [x] are not removed, but simply
           hidden. That is, after performing [Hashtbl.remove tbl x],
           the previous binding for [x], if any, is restored.
           (Same behavior as with association lists.) *)

val find : ('a, 'b) t -> 'a -> 'b
        (* [Hashtbl.find tbl x] returns the current binding of [x] in [tbl],
           or raises [Not_found] if no such binding exists. *)

val find_all : ('a, 'b) t -> 'a -> 'b list
        (* [Hashtbl.find_all tbl x] returns the list of all data
           associated with [x] in [tbl].
           The current binding is returned first, then the previous
           bindings, in reverse order of introduction in the table. *)

val mem :  ('a, 'b) t -> 'a -> bool
        (* [Hashtbl.mem tbl x] checks if [x] is bound in [tbl]. *)

val remove : ('a, 'b) t -> 'a -> unit
        (* [Hashtbl.remove tbl x] removes the current binding of [x] in [tbl],
           restoring the previous binding if it exists.
           It does nothing if [x] is not bound in [tbl]. *)

val replace : ('a, 'b) t -> 'a -> 'b -> unit
        (* [Hashtbl.replace tbl x y] replaces the current binding of [x]
           in [tbl] by a binding of [x] to [y].  If [x] is unbound in [tbl],
           a binding of [x] to [y] is added to [tbl].
           This is functionally equivalent to [Hashtbl.remove tbl x]
           followed by [Hashtbl.add tbl x y]. *)

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
        (* [Hashtbl.iter f tbl] applies [f] to all bindings in table [tbl].
           [f] receives the key as first argument, and the associated value
           as second argument. The order in which the bindings are passed to
           [f] is unspecified. Each binding is presented exactly once
           to [f]. *)

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
        (* [Hashtbl.fold f tbl init] computes
           [(f kN dN ... (f k1 d1 init)...)],
           where [k1 ... kN] are the keys of all bindings in [tbl],
           and [d1 ... dN] are the associated values.
           The order in which the bindings are passed to
           [f] is unspecified. Each binding is presented exactly once
           to [f]. *)

val length : ('a, 'b) t -> int

(*** Functorial interface *)

module type HashedType =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
  end
        (* The input signature of the functor [Hashtbl.Make].
           [t] is the type of keys.
           [equal] is the equality predicate used to compare keys.
           [hash] is a hashing function on keys, returning a non-negative
           integer. It must be such that if two keys are equal according
           to [equal], then they must have identical hash values as computed
           by [hash].
           Examples: suitable ([equal], [hash]) pairs for arbitrary key
           types include
           ([(=)], [Hashtbl.hash]) for comparing objects by structure, and
           ([(==)], [Hashtbl.hash]) for comparing objects by addresses
           (e.g. for mutable or cyclic keys). *)

module type S =
  sig
    type key
    type 'a t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
  end

module Make(H : HashedType) : (S with type key = H.t)

        (* The functor [Hashtbl.Make] returns a structure containing
           a type [key] of keys and a type ['a t] of hash tables
           associating data of type ['a] to keys of type [key].
           The operations perform similarly to those of the generic
           interface, but use the hashing and equality functions
           specified in the functor argument [H] instead of generic
           equality and hashing. *)

(*** The polymorphic hash primitive *)

val hash : 'a -> int
        (* [Hashtbl.hash x] associates a positive integer to any value of
           any type. It is guaranteed that
                if [x = y], then [hash x = hash y]. 
           Moreover, [hash] always terminates, even on cyclic
           structures. *)

external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"
        (* [Hashtbl.hash_param n m x] computes a hash value for [x], with the
           same properties as for [hash]. The two extra parameters [n] and
           [m] give more precise control over hashing. Hashing performs a
           depth-first, right-to-left traversal of the structure [x], stopping
           after [n] meaningful nodes were encountered, or [m] nodes,
           meaningful or not, were encountered. Meaningful nodes are: integers;
           floating-point numbers; strings; characters; booleans; and constant
           constructors. Larger values of [m] and [n] means that more
           nodes are taken into account to compute the final hash
           value, and therefore collisions are less likely to happen.
           However, hashing takes longer. The parameters [m] and [n]
           govern the tradeoff between accuracy and speed. *)
end

module Lexing : sig
(* Module [Lexing]: the run-time library for lexers generated by [ocamllex] *)

(*** Lexer buffers *)

type lexbuf

val from_string : string -> lexbuf
        (* Create a lexer buffer which reads from
           the given string. Reading starts from the first character in
           the string. An end-of-input condition is generated when the
           end of the string is reached. *)
val from_function : (string -> int -> int) -> lexbuf
        (* Create a lexer buffer with the given function as its reading method.
           When the scanner needs more characters, it will call the given
           function, giving it a character string [s] and a character
           count [n]. The function should put [n] characters or less in [s],
           starting at character number 0, and return the number of characters
           provided. A return value of 0 means end of input. *)

(*** Functions for lexer semantic actions *)

        (* The following functions can be called from the semantic actions
           of lexer definitions (the ML code enclosed in braces that
           computes the value returned by lexing functions). They give
           access to the character string matched by the regular expression
           associated with the semantic action. These functions must be
           applied to the argument [lexbuf], which, in the code generated by
           [ocamllex], is bound to the lexer buffer passed to the parsing
           function. *)

val lexeme : lexbuf -> string
        (* [Lexing.lexeme lexbuf] returns the string matched by
           the regular expression. *)
val lexeme_char : lexbuf -> int -> char
        (* [Lexing.lexeme_char lexbuf i] returns character number [i] in
           the matched string. *)
val lexeme_start : lexbuf -> int
        (* [Lexing.lexeme_start lexbuf] returns the position in the
           input stream of the first character of the matched string.
           The first character of the stream has position 0. *)
val lexeme_end : lexbuf -> int
        (* [Lexing.lexeme_end lexbuf] returns the position in the input stream
           of the character following the last character of the matched
           string. The first character of the stream has position 0. *)

end

module List : sig
(* Module [List]: list operations *)

(* Some functions are flagged as not tail-recursive.  A tail-recursive
   function uses constant stack space, while a non-tail-recursive function
   uses stack space proportional to the length of its list argument, which
   can be a problem with very long lists.  When the function takes several
   list arguments, an approximate formula giving stack usage (in some
   unspecified constant unit) is shown in parentheses.

   The above considerations can usually be ignored if your lists are not
   longer than about 10000 elements.
*)

val length : 'a list -> int
        (* Return the length (number of elements) of the given list. *)
val hd : 'a list -> 'a
        (* Return the first element of the given list. Raise
           [Failure "hd"] if the list is empty. *)
val tl : 'a list -> 'a list
        (* Return the given list without its first element. Raise
           [Failure "tl"] if the list is empty. *)
val nth : 'a list -> int -> 'a
        (* Return the n-th element of the given list.
           The first element (head of the list) is at position 0.
           Raise [Failure "nth"] if the list is too short. *)
val rev : 'a list -> 'a list
        (* List reversal. *)
val append : 'a list -> 'a list -> 'a list
        (* Catenate two lists.  Same function as the infix operator [@].
           Not tail-recursive.  The [@] operator is not tail-recursive
           either. *)
val rev_append : 'a list -> 'a list -> 'a list
        (* [List.rev_append l1 l2] reverses [l1] and catenates it to [l2].
           This is equivalent to [List.rev l1 @ l2], but [rev_append] is
           tail-recursive and more efficient. *)
val concat  : 'a list list -> 'a list
val flatten : 'a list list -> 'a list
        (* Catenate (flatten) a list of lists.  Not tail-recursive
           (length of the argument + length of the longest sub-list). *)

(** Iterators *)

val iter : ('a -> unit) -> 'a list -> unit
        (* [List.iter f [a1; ...; an]] applies function [f] in turn to
           [a1; ...; an]. It is equivalent to
           [begin f a1; f a2; ...; f an; () end]. *)
val map : ('a -> 'b) -> 'a list -> 'b list
        (* [List.map f [a1; ...; an]] applies function [f] to [a1, ..., an],
           and builds the list [[f a1; ...; f an]]
           with the results returned by [f].  Not tail-recursive. *)
val rev_map : ('a -> 'b) -> 'a list -> 'b list
        (* [List.rev_map f l] gives the same result as
           [List.rev (List.map f l)], but is tail-recursive and
           more efficient. *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
        (* [List.fold_left f a [b1; ...; bn]] is
           [f (... (f (f a b1) b2) ...) bn]. *)
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
        (* [List.fold_right f [a1; ...; an] b] is
           [f a1 (f a2 (... (f an b) ...))].  Not tail-recursive. *)

(** Iterators on two lists *)

val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
        (* [List.iter2 f [a1; ...; an] [b1; ...; bn]] calls in turn
           [f a1 b1; ...; f an bn].
           Raise [Invalid_argument] if the two lists have
           different lengths. *)
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
        (* [List.map2 f [a1; ...; an] [b1; ...; bn]] is
           [[f a1 b1; ...; f an bn]].
           Raise [Invalid_argument] if the two lists have
           different lengths.  Not tail-recursive. *)
val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
        (* [List.rev_map2 f l] gives the same result as
           [List.rev (List.map2 f l)], but is tail-recursive and
           more efficient. *)
val fold_left2 :
        ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
        (* [List.fold_left2 f a [b1; ...; bn] [c1; ...; cn]] is
           [f (... (f (f a b1 c1) b2 c2) ...) bn cn].
           Raise [Invalid_argument] if the two lists have
           different lengths. *)
val fold_right2 :
        ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
        (* [List.fold_right2 f [a1; ...; an] [b1; ...; bn] c] is
           [f a1 b1 (f a2 b2 (... (f an bn c) ...))].
           Raise [Invalid_argument] if the two lists have
           different lengths.  Not tail-recursive. *)

(** List scanning *)

val for_all : ('a -> bool) -> 'a list -> bool
        (* [for_all p [a1; ...; an]] checks if all elements of the list
           satisfy the predicate [p]. That is, it returns
           [(p a1) && (p a2) && ... && (p an)]. *)
val exists : ('a -> bool) -> 'a list -> bool
        (* [exists p [a1; ...; an]] checks if at least one element of
           the list satisfies the predicate [p]. That is, it returns
           [(p a1) || (p a2) || ... || (p an)]. *)
val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
        (* Same as [for_all] and [exists], but for a two-argument predicate.
           Raise [Invalid_argument] if the two lists have
           different lengths. *)
val mem : 'a -> 'a list -> bool
        (* [mem a l] is true if and only if [a] is equal
           to an element of [l]. *)
val memq : 'a -> 'a list -> bool
        (* Same as [mem], but uses physical equality instead of structural
           equality to compare list elements. *)

(** List searching *)

val find : ('a -> bool) -> 'a list -> 'a
        (* [find p l] returns the first element of the list [l]
           that satisfies the predicate [p].
           Raise [Not_found] if there is no value that satisfies [p] in the
           list [l]. *)

val filter : ('a -> bool) -> 'a list -> 'a list
val find_all : ('a -> bool) -> 'a list -> 'a list
        (* [filter p l] returns all the elements of the list [l]
           that satisfies the predicate [p].  The order of the elements
           in the input list is preserved.  [find_all] is another name
           for [filter]. *)

val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
        (* [partition p l] returns a pair of lists [(l1, l2)], where
           [l1] is the list of all the elements of [l] that
           satisfy the predicate [p], and [l2] is the list of all the
           elements of [l] that do not satisfy [p].
           The order of the elements in the input list is preserved. *)

(** Association lists *)

val assoc : 'a -> ('a * 'b) list -> 'b
        (* [assoc a l] returns the value associated with key [a] in the list of
           pairs [l]. That is,
             [assoc a [ ...; (a,b); ...] = b]
           if [(a,b)] is the leftmost binding of [a] in list [l].
           Raise [Not_found] if there is no value associated with [a] in the
           list [l]. *)
val assq : 'a -> ('a * 'b) list -> 'b
        (* Same as [assoc], but uses physical equality instead of structural
           equality to compare keys. *)

val mem_assoc : 'a -> ('a * 'b) list -> bool
        (* Same as [assoc], but simply return true if a binding exists,
           and false if no bindings exist for the given key. *)
val mem_assq : 'a -> ('a * 'b) list -> bool
        (* Same as [mem_assoc], but uses physical equality instead of
           structural equality to compare keys. *)

val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
        (* [remove_assoc a l] returns the list of
           pairs [l] without the first pair with key [a], if any.
           Not tail-recursive. *)

val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
        (* Same as [remove_assq], but uses physical equality instead
           of structural equality to compare keys.  Not tail-recursive. *)

(** Lists of pairs *)

val split : ('a * 'b) list -> 'a list * 'b list
        (* Transform a list of pairs into a pair of lists:
           [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])].
           Not tail-recursive.
        *)
val combine : 'a list -> 'b list -> ('a * 'b) list
        (* Transform a pair of lists into a list of pairs:
           [combine ([a1; ...; an], [b1; ...; bn])] is
              [[(a1,b1); ...; (an,bn)]].
           Raise [Invalid_argument] if the two lists
           have different lengths.  Not tail-recursive. *)

(** Sorting *)
val sort : ('a -> 'a -> int) -> 'a list -> 'a list;;
        (* Sort a list in increasing order according to a comparison
           function.  The comparison function must return 0 if it arguments
           compare as equal, a positive integer if the first is greater,
           and a negative integer if the first is smaller.  For example,
           the [compare] function is a suitable comparison function.
           The resulting list is sorted in increasing order.
           [List.sort] is guaranteed to run in constant heap space
           (in addition to the size of the result list) and logarithmic
           stack space.

           The current implementation uses Merge Sort and is the same as
           [List.stable_sort].
        *)
val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list;;
        (* Same as [List.sort], but the sorting algorithm is stable.
           The current implementation is Merge Sort. It runs in constant
           heap space.
        *)
end

module Sort : sig
(* Module [Sort]: sorting and merging lists *)

(* This module is obsolete and exists only for backward compatibility.
   The sorting functions in [Array] and [List] should be used instead.
   The new functions are faster and use less memory.
*)

val list : ('a -> 'a -> bool) -> 'a list -> 'a list
        (* Sort a list in increasing order according to an ordering predicate.
           The predicate should return [true] if its first argument is
           less than or equal to its second argument. *)

val array : ('a -> 'a -> bool) -> 'a array -> unit
        (* Sort an array in increasing order according to an
           ordering predicate.
           The predicate should return [true] if its first argument is
           less than or equal to its second argument.
           The array is sorted in place. *)

val merge : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
        (* Merge two lists according to the given predicate.
           Assuming the two argument lists are sorted according to the
           predicate, [merge] returns a sorted list containing the elements
           from the two lists. The behavior is undefined if the two
           argument lists were not sorted. *)
end

module Map : sig
(* Module [Map]: association tables over ordered types *)

(* This module implements applicative association tables, also known as
   finite maps or dictionaries, given a total ordering function
   over the keys.
   All operations over maps are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map. *)

module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end
          (* The input signature of the functor [Map.Make].
             [t] is the type of the map keys.
             [compare] is a total ordering function over the keys.
             This is a two-argument function [f] such that
             [f e1 e2] is zero if the keys [e1] and [e2] are equal,
             [f e1 e2] is strictly negative if [e1] is smaller than [e2],
             and [f e1 e2] is strictly positive if [e1] is greater than [e2].
             Example: a suitable ordering function is
             the generic structural comparison function [compare]. *)

module type S =
  sig
    type key
    (** The type of the map keys. *)

    type (+'a) t
    (** The type of maps from type [key] to type ['a]. *)

    val empty: 'a t
    (** The empty map. *)

    val is_empty: 'a t -> bool
    (** Test whether a map is empty or not. *)

    val mem: key -> 'a t -> bool
    (** [mem x m] returns [true] if [m] contains a binding for [x],
       and [false] otherwise. *)

    val add: key -> 'a -> 'a t -> 'a t
    (** [add x y m] returns a map containing the same bindings as
       [m], plus a binding of [x] to [y]. If [x] was already bound
       in [m], its previous binding disappears. *)

    val singleton: key -> 'a -> 'a t
    (** [singleton x y] returns the one-element map that contains a binding [y]
        for [x].
        @since 3.12.0
     *)

    val remove: key -> 'a t -> 'a t
    (** [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map. *)

    val merge:
         (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
        and of [m2]. The presence of each such binding, and the corresponding
        value, is determined with the function [f].
        @since 3.12.0
     *)

    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** Total ordering between maps.  The first argument is a total ordering
        used to compare data associated with equal keys in the two maps. *)

    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
       equal, that is, contain equal keys and associate them with
       equal data.  [cmp] is the equality predicate used to compare
       the data associated with the keys. *)

    val iter: (key -> 'a -> unit) -> 'a t -> unit
    (** [iter f m] applies [f] to all bindings in map [m].
       [f] receives the key as first argument, and the associated value
       as second argument.  The bindings are passed to [f] in increasing
       order with respect to the ordering over the type of the keys. *)

    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
       where [k1 ... kN] are the keys of all bindings in [m]
       (in increasing order), and [d1 ... dN] are the associated data. *)

    val for_all: (key -> 'a -> bool) -> 'a t -> bool
    (** [for_all p m] checks if all the bindings of the map
        satisfy the predicate [p].
        @since 3.12.0
     *)

    val exists: (key -> 'a -> bool) -> 'a t -> bool
    (** [exists p m] checks if at least one binding of the map
        satisfy the predicate [p].
        @since 3.12.0
     *)

    val filter: (key -> 'a -> bool) -> 'a t -> 'a t
    (** [filter p m] returns the map with all the bindings in [m]
        that satisfy predicate [p].
        @since 3.12.0
     *)

    val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    (** [partition p m] returns a pair of maps [(m1, m2)], where
        [m1] contains all the bindings of [s] that satisfy the
        predicate [p], and [m2] is the map with all the bindings of
        [s] that do not satisfy [p].
        @since 3.12.0
     *)

    val cardinal: 'a t -> int
    (** Return the number of bindings of a map.
        @since 3.12.0
     *)

    val bindings: 'a t -> (key * 'a) list
    (** Return the list of all bindings of the given map.
       The returned list is sorted in increasing order with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Map.Make}.
        @since 3.12.0
     *)

    val min_binding: 'a t -> (key * 'a)
    (** Return the smallest binding of the given map
       (with respect to the [Ord.compare] ordering), or raise
       [Not_found] if the map is empty.
        @since 3.12.0
     *)

    val max_binding: 'a t -> (key * 'a)
    (** Same as {!Map.S.min_binding}, but returns the largest binding
        of the given map.
        @since 3.12.0
     *)

    val choose: 'a t -> (key * 'a)
    (** Return one binding of the given map, or raise [Not_found] if
       the map is empty. Which binding is chosen is unspecified,
       but equal bindings will be chosen for equal maps.
        @since 3.12.0
     *)

    val split: key -> 'a t -> 'a t * 'a option * 'a t
    (** [split x m] returns a triple [(l, data, r)], where
          [l] is the map with all the bindings of [m] whose key
        is strictly less than [x];
          [r] is the map with all the bindings of [m] whose key
        is strictly greater than [x];
          [data] is [None] if [m] contains no binding for [x],
          or [Some v] if [m] binds [v] to [x].
        @since 3.12.0
     *)

    val find: key -> 'a t -> 'a
    (** [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. *)

    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The bindings are passed to [f] in increasing order
       with respect to the ordering over the type of the keys. *)

    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    (** Same as {!Map.S.map}, but the function receives as arguments both the
       key and the associated value for each binding of the map. *)

  end

module Make(Ord : OrderedType) : (S with type key = Ord.t)
        (* Functor building an implementation of the map structure
           given a totally ordered type. *)
end

module Printexc : sig
(* Module [Printexc]: a catch-all exception handler *)

val to_string : exn -> string
        (* [Printexc.to_string e] returns a string representation of [e]. *)
end

module Buffer : sig
(* Module [Buffer]: extensible string buffers *)

(* This module implements string buffers that automatically expand
   as necessary.  It provides accumulative concatenation of strings
   in quasi-linear time (instead of quadratic time when strings are
   concatenated pairwise). *)

type t
     (* The abstract type of buffers. *)

val create : int -> t
     (* [create n] returns a fresh buffer, initially empty.
        The [n] parameter is the initial size of the internal string
        that holds the buffer contents.  That string is automatically
        reallocated when more than [n] characters are stored in the buffer,
        but shrinks back to [n] characters when [reset] is called.
        For best performance, [n] should be of the same order of magnitude
        as the number of characters that are expected to be stored in
        the buffer (for instance, 80 for a buffer that holds one output
        line).  Nothing bad will happen if the buffer grows beyond that
        limit, however.  In doubt, take [n = 16] for instance.
        If [n] is not between 1 and [Sys.max_string_length], it will
        be clipped to that interval. *)
val contents : t -> string
     (* Return a copy of the current contents of the buffer.
        The buffer itself is unchanged. *)
val length : t -> int
     (* Return the number of characters currently contained in the buffer. *)
val clear : t -> unit
     (* Empty the buffer. *)
val reset : t -> unit
     (* Empty the buffer and deallocate the internal string holding the
        buffer contents, replacing it with the initial internal string
        of length [n] that was allocated by [create n].
        For long-lived buffers that may have grown a lot, [reset] allows
        faster reclaimation of the space used by the buffer. *)
val add_char : t -> char -> unit
     (* [add_char b c] appends the character [c] at the end of
        the buffer [b]. *)
val add_string : t -> string -> unit
     (* [add_string b s] appends the string [s] at the end of
        the buffer [b]. *)
val add_substring : t -> string -> int -> int -> unit
     (* [add_substring b s ofs len] takes [len] characters from offset
        [ofs] in string [s] and appends them at the end of the buffer [b]. *)
val add_buffer : t -> t -> unit
     (* [add_buffer b1 b2] appends the current contents of buffer [b2]
        at the end of buffer [b1].  [b2] is not modified. *)

end

module Printf : sig
(* Module [Printf]: formatting printing functions *)
(*
val sprintf : ('a, unit, string) format -> 'a
        (* Same as [printf], but return the result of formatting in a
           string. *)

val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
        (* Same as [fprintf], but instead of printing on an output channel,
           append the formatted arguments to the given extensible buffer
           (see module [Buffer]). *)
*)
end

module Queue : sig
(* Module [Queue]: first-in first-out queues *)

(* This module implements queues (FIFOs), with in-place modification. *)

type 'a t
        (* The type of queues containing elements of type ['a]. *)

exception Empty
        (* Raised when [take] is applied to an empty queue. *)

val create : unit -> 'a t
        (* Return a new queue, initially empty. *)
val add : 'a -> 'a t -> unit
        (* [add x q] adds the element [x] at the end of the queue [q]. *)
val take : 'a t -> 'a
        (* [take q] removes and returns the first element in queue [q],
           or raises [Empty] if the queue is empty. *)
val peek : 'a t -> 'a
        (* [peek q] returns the first element in queue [q], without removing
           it from the queue, or raises [Empty] if the queue is empty. *)
val clear : 'a t -> unit
        (* Discard all elements from a queue. *)
val length : 'a t -> int
        (* Return the number of elements in a queue. *)
val iter : ('a -> unit) -> 'a t -> unit
        (* [iter f q] applies [f] in turn to all elements of [q],
           from the least recently entered to the most recently entered.
           The queue itself is unchanged. *)
end

module Random : sig
(* Module [Random]: pseudo-random number generator *)

val bits : unit -> int
  (* Return 30 random bits in a nonnegative integer. *)
val int : int -> int
  (* [Random.int bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be more than 0 and less
     than $2^{30}$. *)
val float : float -> float
  (* [Random.float bound] returns a random floating-point number
     between 0 (inclusive) and [bound] (exclusive).  If [bound] is
     negative, the result is negative.  If [bound] is 0, the result
     is 0. *)
end

module Set : sig
(* Module [Set]: sets over ordered types *)

(* This module implements the set data structure, given a total ordering
   function over the set elements. All operations over sets
   are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and is therefore
   reasonably efficient: insertion and membership take time
   logarithmic in the size of the set, for instance. *)

module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end
          (* The input signature of the functor [Set.Make].
             [t] is the type of the set elements.
             [compare] is a total ordering function over the set elements.
             This is a two-argument function [f] such that
             [f e1 e2] is zero if the elements [e1] and [e2] are equal,
             [f e1 e2] is strictly negative if [e1] is smaller than [e2],
             and [f e1 e2] is strictly positive if [e1] is greater than [e2].
             Example: a suitable ordering function is
             the generic structural comparison function [compare]. *)

module type S =
  sig
    type elt
          (* The type of the set elements. *)
    type t
          (* The type of sets. *)
    val empty : t
          (* The empty set. *)
    val is_empty : t -> bool
        (* Test whether a set is empty or not. *)
    val mem : elt -> t -> bool
        (* [mem x s] tests whether [x] belongs to the set [s]. *)
    val add : elt -> t -> t
        (* [add x s] returns a set containing all elements of [s],
           plus [x]. If [x] was already in [s], [s] is returned unchanged. *)
    val singleton : elt -> t
        (* [singleton x] returns the one-element set containing only [x]. *)
    val remove : elt -> t -> t
        (* [remove x s] returns a set containing all elements of [s],
           except [x]. If [x] was not in [s], [s] is returned unchanged. *)
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
        (* Union, intersection and set difference. *)
    val compare : t -> t -> int
        (* Total ordering between sets. Can be used as the ordering function
           for doing sets of sets. *)
    val equal : t -> t -> bool
        (* [equal s1 s2] tests whether the sets [s1] and [s2] are
           equal, that is, contain equal elements. *)
    val subset : t -> t -> bool
        (* [subset s1 s2] tests whether the set [s1] is a subset of
           the set [s2]. *)
    val iter : (elt -> unit) -> t -> unit
        (* [iter f s] applies [f] in turn to all elements of [s].
           The order in which the elements of [s] are presented to [f]
           is unspecified. *)
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
        (* [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
           where [x1 ... xN] are the elements of [s].
           The order in which elements of [s] are presented to [f] is
           unspecified. *)
    val for_all : (elt -> bool) -> t -> bool
        (* [for_all p s] checks if all elements of the set
           satisfy the predicate [p]. *)
    val exists : (elt -> bool) -> t -> bool
        (* [exists p s] checks if at least one element of
           the set satisfies the predicate [p]. *)
    val filter : (elt -> bool) -> t -> t
        (* [filter p s] returns the set of all elements in [s]
           that satisfy predicate [p]. *)
    val partition : (elt -> bool) -> t -> t * t
        (* [partition p s] returns a pair of sets [(s1, s2)], where
           [s1] is the set of all the elements of [s] that satisfy the
           predicate [p], and [s2] is the set of all the elements of
           [s] that do not satisfy [p]. *)
    val cardinal : t -> int
        (* Return the number of elements of a set. *)
    val elements : t -> elt list
        (* Return the list of all elements of the given set.
           The returned list is sorted in increasing order with respect
           to the ordering [Ord.compare], where [Ord] is the argument
           given to [Set.Make]. *)
    val min_elt : t -> elt
        (* Return the smallest element of the given set
           (with respect to the [Ord.compare] ordering), or raise
           [Not_found] if the set is empty. *)
    val max_elt : t -> elt
        (* Same as [min_elt], but returns the largest element of the
           given set. *)
    val choose : t -> elt
        (* Return one element of the given set, or raise [Not_found] if
           the set is empty. Which element is chosen is unspecified,
           but equal elements will be chosen for equal sets. *)
    val split: elt -> t -> t * bool * t
  end

module Make(Ord : OrderedType) : (S with type elt = Ord.t)
        (* Functor building an implementation of the set structure
           given a totally ordered type. *)
end

module Stack : sig
(* Module [Stack]: last-in first-out stacks *)

(* This module implements stacks (LIFOs), with in-place modification. *)

type 'a t
        (* The type of stacks containing elements of type ['a]. *)

exception Empty
        (* Raised when [pop] is applied to an empty stack. *)

val create : unit -> 'a t
        (* Return a new stack, initially empty. *)
val push : 'a -> 'a t -> unit
        (* [push x s] adds the element [x] at the top of stack [s]. *)
val pop : 'a t -> 'a
        (* [pop s] removes and returns the topmost element in stack [s],
           or raises [Empty] if the stack is empty. *)
val top : 'a t -> 'a
        (* [top s] returns the topmost element in stack [s],
           or raises [Empty] if the stack is empty. *)
val clear : 'a t -> unit
        (* Discard all elements from a stack. *)
val length : 'a t -> int
        (* Return the number of elements in a stack. *)
val iter : ('a -> unit) -> 'a t -> unit
        (* [iter f s] applies [f] in turn to all elements of [s],
           from the element at the top of the stack to the element at the
           bottom of the stack. The stack itself is unchanged. *)
end

module Stream : sig
(* Module [Stream]: streams and parsers *)

type 'a t
        (* The type of streams holding values of type ['a]. *)

exception Failure;;
        (* Raised by parsers when none of the first components of the stream
           patterns is accepted. *)
exception Error of string;;
        (* Raised by parsers when the first component of a stream pattern is
           accepted, but one of the following components is rejected. *)

(** Stream builders *)
(* Warning: these functions create streams with fast access; it is illegal
   to mix them with streams built with [[< >]]; would raise [Failure]
   when accessing such mixed streams. *)

val from : (int -> 'a option) -> 'a t;;
        (* [Stream.from f] returns a stream built from the function [f].
           To create a new stream element, the function [f] is called with
           the current stream count. The user function [f] must return either
           [Some <value>] for a value or [None] to specify the end of the
           stream. *)
val of_list : 'a list -> 'a t;;
        (* Return the stream holding the elements of the list in the same
           order. *)
val of_string : string -> char t;;
        (* Return the stream of the characters of the string parameter. *)

(** Stream iterator *)

val iter : ('a -> unit) -> 'a t -> unit;;
        (* [Stream.iter f s] scans the whole stream s, applying function [f]
           in turn to each stream element encountered. *)

(** Predefined parsers *)

val next : 'a t -> 'a;;
        (* Return the first element of the stream and remove it from the
           stream. Raise [Stream.Failure] if the stream is empty. *)
val empty : 'a t -> unit;;
        (* Return [()] if the stream is empty, else raise [Stream.Failure]. *)

(** Useful functions *)

val peek : 'a t -> 'a option;;
        (* Return [Some] of "the first element" of the stream, or [None] if
           the stream is empty. *)
val junk : 'a t -> unit;;
        (* Remove the first element of the stream, possibly unfreezing
           it before. *)
val count : 'a t -> int;;
        (* Return the current count of the stream elements, i.e. the number
           of the stream elements discarded. *)

val npeek : int -> 'a t -> 'a list;;
        (* [npeek n] returns the list of the [n] first elements of
           the stream, or all its remaining elements if less than [n]
           elements are available. *)

end


module String : sig
(* Module [String]: string operations *)

external length : string -> int = "%string_length"
        (* Return the length (number of characters) of the given string. *)

external get : string -> int -> char = "%string_safe_get"
        (* [String.get s n] returns character number [n] in string [s].
           The first character is character number 0.
           The last character is character number [String.length s - 1].
           Raise [Invalid_argument] if [n] is outside the range
           0 to [(String.length s - 1)].
           You can also write [s.[n]] instead of [String.get s n]. *)
external set : string -> int -> char -> unit = "%string_safe_set"
        (* [String.set s n c] modifies string [s] in place,
           replacing the character number [n] by [c].
           Raise [Invalid_argument] if [n] is outside the range
           0 to [(String.length s - 1)].
           You can also write [s.[n] <- c] instead of [String.set s n c]. *)

val make : int -> char -> string
        (* [String.make n c] returns a fresh string of length [n],
           filled with the character [c].
           Raise [Invalid_argument] if [n <= 0] or [n > Sys.max_string_length].
           *)
val copy : string -> string
        (* Return a copy of the given string. *)
val sub : string -> int -> int -> string
        (* [String.sub s start len] returns a fresh string of length [len],
           containing the characters number [start] to [start + len - 1]
           of string [s].
           Raise [Invalid_argument] if [start] and [len] do not
           designate a valid substring of [s]; that is, if [start < 0],
           or [len < 0], or [start + len > String.length s]. *)
val fill : string -> int -> int -> char -> unit
        (* [String.fill s start len c] modifies string [s] in place,
           replacing the characters number [start] to [start + len - 1]
           by [c].
           Raise [Invalid_argument] if [start] and [len] do not
           designate a valid substring of [s]. *)
val blit : string -> int ->
           string -> int -> int -> unit
        (* [String.blit src srcoff dst dstoff len] copies [len] characters
           from string [src], starting at character number [srcoff], to
           string [dst], starting at character number [dstoff]. It works
           correctly even if [src] and [dst] are the same string,
           and the source and destination chunks overlap.
           Raise [Invalid_argument] if [srcoff] and [len] do not
           designate a valid substring of [src], or if [dstoff] and [len]
           do not designate a valid substring of [dst]. *)

val concat : string -> string list -> string
        (* [String.concat sep sl] catenates the list of strings [sl],
           inserting the separator string [sep] between each. *)

val escaped : string -> string
        (* Return a copy of the argument, with special characters represented
           by escape sequences, following the lexical conventions of
           Objective Caml. *)

val index : string -> char -> int
        (* [String.index s c] returns the position of the leftmost
           occurrence of character [c] in string [s].
           Raise [Not_found] if [c] does not occur in [s]. *)
val rindex : string -> char -> int
        (* [String.rindex s c] returns the position of the rightmost
           occurrence of character [c] in string [s].
           Raise [Not_found] if [c] does not occur in [s]. *)
val index_from : string -> int -> char -> int
val rindex_from : string -> int -> char -> int
        (* Same as [String.index] and [String.rindex], but start
           searching at the character position given as second argument.
           [String.index s c] is equivalent to [String.index_from s 0 c],
           and [String.rindex s c] to
           [String.rindex_from s (String.length s - 1) c]. *)

val contains : string -> char -> bool
        (* [String.contains s c] tests if character [c]
           appears in the string [s]. *)
val contains_from : string -> int -> char -> bool
        (* [String.contains_from s start c] tests if character [c]
           appears in the substring of [s] starting from [start] to the end
           of [s].
           Raise [Invalid_argument] if [start] is not a valid index of [s]. *)
val rcontains_from : string -> int -> char -> bool
        (* [String.rcontains_from s stop c] tests if character [c]
           appears in the substring of [s] starting from the beginning
           of [s] to index [stop].
           Raise [Invalid_argument] if [stop] is not a valid index of [s]. *)

val uppercase : string -> string
        (* Return a copy of the argument, with all lowercase letters
           translated to uppercase, including accented letters of the ISO
           Latin-1 (8859-1) character set. *)
val lowercase : string -> string
        (* Return a copy of the argument, with all uppercase letters
           translated to lowercase, including accented letters of the ISO
           Latin-1 (8859-1) character set. *)
val capitalize : string -> string
        (* Return a copy of the argument, with the first letter
           set to uppercase. *)
val uncapitalize : string -> string
        (* Return a copy of the argument, with the first letter
           set to lowercase. *)

end

module Genlex : sig
(* Module [Genlex]: a generic lexical analyzer *)

(* This module implements a simple ``standard'' lexical analyzer, presented
   as a function from character streams to token streams. It implements
   roughly the lexical conventions of Caml, but is parameterized by the
   set of keywords of your language. *)

type token =
    Kwd of string
  | Ident of string
  | Int of int
  | Float of float
  | String of string
  | Char of char
        (* The type of tokens. The lexical classes are: [Int] and [Float]
           for integer and floating-point numbers; [String] for
           string literals, enclosed in double quotes; [Char] for
           character literals, enclosed in single quotes; [Ident] for
           identifiers (either sequences of letters, digits, underscores
           and quotes, or sequences of ``operator characters'' such as
           [+], [*], etc); and [Kwd] for keywords (either identifiers or
           single ``special characters'' such as [(], [}], etc). *)
           
val make_lexer : string list -> (char Stream.t -> token Stream.t)
        (* Construct the lexer function. The first argument is the list of
           keywords. An identifier [s] is returned as [Kwd s] if [s]
           belongs to this list, and as [Ident s] otherwise.
           A special character [s] is returned as [Kwd s] if [s]
           belongs to this list, and cause a lexical error (exception
           [Parse_error]) otherwise. Blanks and newlines are skipped.
           Comments delimited by [(*] and [*)] are skipped as well,
           and can be nested. *)

        (* Example: a lexer suitable for a desk calculator is obtained by
           [
           let lexer = make_lexer ["+";"-";"*";"/";"let";"="; "("; ")"]
           ]
           The associated parser would be a function from [token stream]
           to, for instance, [int], and would have rules such as:
           [
           let parse_expr = parser
                  [< 'Int n >] -> n
                | [< 'Kwd "("; n = parse_expr; 'Kwd ")" >] -> n
                | [< n1 = parse_expr; n2 = parse_remainder n1 >] -> n2
           and parse_remainder n1 = parser
                  [< 'Kwd "+"; n2 = parse_expr >] -> n1+n2
                | ...
           ]
*)
end
module Widget : sig
  type widget
	  (* widget is an abstract type *)
  val name : widget -> string
	(* Returns the name (tk "path") of a widget *)
  exception IllegalWidgetType of string
end
module Protocol : sig
  exception TkError of string
  end

(* Support for Tk -textvariable option *)
module Textvariable : sig
open Widget
type textVariable
      (* TextVariable is an abstract type *)

val create : unit -> textVariable
      (* Allocation of a textVariable *)
val create_temporary : widget -> textVariable
      (* Allocation of a textVariable with lifetime associated to widget *)
val set : textVariable -> string -> unit
      (* Setting the val of a textVariable *)
val get : textVariable -> string
      (* Reading the val of a textVariable *)
val name : textVariable -> string
      (* Its tcl name *)

end
module Timer : sig
  type t

  val add : int -> (unit -> unit) -> t
  val set : int -> (unit -> unit) -> unit
  val remove : t -> unit
end
module Tk : sig
open Widget
open Textvariable
type bitmap =
   BitmapFile of string                 (* path of file *)
 | Predefined of string                 (* bitmap  name *)
type color =
     NamedColor of string
   | Black			(* tk keyword: black *)
   | White			(* tk keyword: white *)
   | Red			(* tk keyword: red *)
   | Green			(* tk keyword: green *)
   | Blue			(* tk keyword: blue *)
   | Yellow                     (* tk keyword: yellow *)
type cursor =
   XCursor of string 
 | XCursorFg of string * color
 | XCursortFgBg of string * color * color
 | CursorFileFg of string * color 
 | CursorMaskFile of string * string * color * color
type units =
    Pixels of int       (* specified as floating-point, but inconvenient *)
  | Centimeters of float
  | Inches of float
  | Millimeters of float
  | PrinterPoint of float
type scrollValue =
	ScrollPage of int		(* tk option: scroll <int> page *)
	| ScrollUnit of int		(* tk option: scroll <int> unit *)
	| MoveTo of float		(* tk option: moveto <float> *)
type xEvent =
    ButtonPress (* also Button, but we omit it *)
  | ButtonPressDetail of int
  | ButtonRelease
  | ButtonReleaseDetail of int
  | Circulate
  | ColorMap
  | Configure
  | Destroy
  | Enter
  | Expose
  | FocusIn
  | FocusOut
  | Gravity
  | KeyPress (* also Key, but we omit it *)
  | KeyPressDetail of string      (* /usr/include/X11/keysymdef.h *)
  | KeyRelease
  | KeyReleaseDetail of string
  | Leave
  | Map
  | Motion
  | Property
  | Reparent
  | Unmap
  | Visibility 
type modifier =
    Control
  | Shift
  | Lock
  | Button1
  | Button2
  | Button3
  | Button4
  | Button5
  | Double
  | Triple
  | Mod1
  | Mod2
  | Mod3
  | Mod4
  | Mod5
  | Meta
  | Alt 
type eventInfo =
  {
  mutable ev_Above : int;               (* tk: %a *)
  mutable ev_ButtonNumber : int;        (* tk: %b *)
  mutable ev_Count : int;               (* tk: %c *)
  mutable ev_Detail : string;           (* tk: %d *)
  mutable ev_Focus : bool;              (* tk: %f *)
  mutable ev_Height : int;              (* tk: %h *)
  mutable ev_KeyCode : int;             (* tk: %k *)
  mutable ev_Mode : string;             (* tk: %m *)
  mutable ev_OverrideRedirect : bool;   (* tk: %o *)
  mutable ev_Place : string;            (* tk: %p *)
  mutable ev_State : string;            (* tk: %s *)
  mutable ev_Time : int;                (* tk: %t *)
  mutable ev_Width : int;               (* tk: %w *)
  mutable ev_MouseX : int;              (* tk: %x *)
  mutable ev_MouseY : int;              (* tk: %y *)
  mutable ev_Char : string;             (* tk: %A *)
  mutable ev_BorderWidth : int;         (* tk: %B *)
  mutable ev_SendEvent : bool;          (* tk: %E *)
  mutable ev_KeySymString : string;     (* tk: %K *)
  mutable ev_KeySymInt : int;           (* tk: %N *)
  mutable ev_RootWindow : int;          (* tk: %R *)
  mutable ev_SubWindow : int;           (* tk: %S *)
  mutable ev_Type : int;                (* tk: %T *)
  mutable ev_Widget : widget;           (* tk: %W *)
  mutable ev_RootX : int;               (* tk: %X *)
  mutable ev_RootY : int                (* tk: %Y *)
  }
type eventField =
    Ev_Above
  | Ev_ButtonNumber
  | Ev_Count
  | Ev_Detail
  | Ev_Focus
  | Ev_Height
  | Ev_KeyCode
  | Ev_Mode
  | Ev_OverrideRedirect
  | Ev_Place
  | Ev_State
  | Ev_Time 
  | Ev_Width
  | Ev_MouseX
  | Ev_MouseY
  | Ev_Char
  | Ev_BorderWidth
  | Ev_SendEvent
  | Ev_KeySymString
  | Ev_KeySymInt
  | Ev_RootWindow
  | Ev_SubWindow
  | Ev_Type
  | Ev_Widget
  | Ev_RootX
  | Ev_RootY
type bindAction =
   BindSet of eventField list *  (eventInfo -> unit)
 | BindSetBreakable of eventField list *  (eventInfo -> unit)
 | BindRemove
 | BindExtend of eventField list *  (eventInfo -> unit)
type bindings =
	TagBindings of string		(* tk option: <string> *)
	| WidgetBindings of widget	(* tk option: <widget> *)
type index =
	  Number of int		(* no keyword  *)
        | ActiveElement         (* tk keyword: active *)
	| End		        (* tk keyword: end *)
        | Last			(* tk keyword: last *)
        | NoIndex		(* tk keyword: none *)
	| Insert		(* tk keyword: insert *)
	| SelFirst		(* tk keyword: sel.first *)
	| SelLast		(* tk keyword: sel.last *)
        | At of int		(* tk keyword: @n *)
        | AtXY of int * int     (* tk keyword: @x,y *)
        | AnchorPoint   	(* tk keyword: anchor *)
        | Pattern of string     (* no keyword *)
        | LineChar of int * int (* tk keyword: l.c *)
        | Mark of string        (* no keyword *)
        | TagFirst of string    (* tk keyword: tag.first *)
        | TagLast of string     (* tk keyword: tag.last *)
        | Embedded of widget	(* no keyword *)
type paletteType =
    GrayShades of int
  | RGBShades of int * int * int
type textMark = string
type textTag = string
type textModifier =
    CharOffset of int		(* tk keyword: +/- Xchars *)
  | LineOffset of int		(* tk keyword: +/- Xlines *)
  | LineStart			(* tk keyword: linestart *)
  | LineEnd			(* tk keyword: lineend *)
  | WordStart			(* tk keyword: wordstart *)
  | WordEnd 			(* tk keyword: wordend *)
type textIndex =
   TextIndex of index * textModifier list
 | TextIndexNone
type anchor =
	Center		(* tk option: center *)
	| E		(* tk option: e *)
	| N		(* tk option: n *)
	| NE		(* tk option: ne *)
	| NW		(* tk option: nw *)
	| S		(* tk option: s *)
	| SE		(* tk option: se *)
	| SW		(* tk option: sw *)
	| W		(* tk option: w *)
type imageBitmap =
	BitmapImage of string		(* tk option: <string> *)
type imagePhoto =
	PhotoImage of string		(* tk option: <string> *)
type justification =
	Justify_Center		(* tk option: center *)
	| Justify_Left		(* tk option: left *)
	| Justify_Right		(* tk option: right *)
type orientation =
	Horizontal		(* tk option: horizontal *)
	| Vertical		(* tk option: vertical *)
type relief =
	Flat		(* tk option: flat *)
	| Groove		(* tk option: groove *)
	| Raised		(* tk option: raised *)
	| Ridge		(* tk option: ridge *)
	| Sunken		(* tk option: sunken *)
type state =
	Active		(* tk option: active *)
	| Disabled		(* tk option: disabled *)
	| Normal		(* tk option: normal *)
type colorMode =
	Color		(* tk option: color *)
	| Gray		(* tk option: gray *)
	| Mono		(* tk option: mono *)
type arcStyle =
	Arc		(* tk option: arc *)
	| Chord		(* tk option: chord *)
	| PieSlice		(* tk option: pieslice *)
type tagOrId =
	Id of int		(* tk option: <int> *)
	| Tag of string		(* tk option: <string> *)
type arrowStyle =
	Arrow_Both		(* tk option: both *)
	| Arrow_First		(* tk option: first *)
	| Arrow_Last		(* tk option: last *)
	| Arrow_None		(* tk option: none *)
type capStyle =
	Cap_Butt		(* tk option: butt *)
	| Cap_Projecting		(* tk option: projecting *)
	| Cap_Round		(* tk option: round *)
type joinStyle =
	Join_Bevel		(* tk option: bevel *)
	| Join_Miter		(* tk option: miter *)
	| Join_Round		(* tk option: round *)
type visual =
	Best		(* tk option: best *)
	| BestDepth of int		(* tk option: {best <int>} *)
	| ClassVisual of string * int		(* tk option: {<string> <int>} *)
	| DefaultVisual		(* tk option: default *)
	| WidgetVisual of widget		(* tk option: <widget> *)
type colormap =
	NewColormap		(* tk option: new *)
	| WidgetColormap of widget		(* tk option: <widget> *)
type selectModeType =
	Browse		(* tk option: browse *)
	| Extended		(* tk option: extended *)
	| Multiple		(* tk option: multiple *)
	| Single		(* tk option: single *)
type fillMode =
	Fill_Both		(* tk option: both *)
	| Fill_None		(* tk option: none *)
	| Fill_X		(* tk option: x *)
	| Fill_Y		(* tk option: y *)
type side =
	Side_Bottom		(* tk option: bottom *)
	| Side_Left		(* tk option: left *)
	| Side_Right		(* tk option: right *)
	| Side_Top		(* tk option: top *)
type borderMode =
	Ignore		(* tk option: ignore *)
	| Inside		(* tk option: inside *)
	| Outside		(* tk option: outside *)
type alignType =
	Align_Baseline		(* tk option: baseline *)
	| Align_Bottom		(* tk option: bottom *)
	| Align_Center		(* tk option: center *)
	| Align_Top		(* tk option: top *)
type wrapMode =
	WrapChar		(* tk option: char *)
	| WrapNone		(* tk option: none *)
	| WrapWord		(* tk option: word *)
type tabType =
	TabCenter of units		(* tk option: <units> center *)
	| TabLeft of units		(* tk option: <units> left *)
	| TabNumeric of units		(* tk option: <units> numeric *)
	| TabRight of units		(* tk option: <units> right *)
type options =
	Accelerator of string		(* tk option: -accelerator <string> *)
	| ActiveBackground of color		(* tk option: -activebackground <color> *)
	| ActiveBorderWidth of units		(* tk option: -activeborderwidth <units> *)
	| ActiveForeground of color		(* tk option: -activeforeground <color> *)
	| ActiveRelief of relief		(* tk option: -activerelief <relief> *)
	| After of widget		(* tk option: -after <widget> *)
	| Align of alignType		(* tk option: -align <alignType> *)
	| Anchor of anchor		(* tk option: -anchor <anchor> *)
	| ArcStyle of arcStyle		(* tk option: -style <arcStyle> *)
	| ArrowShape of units * units * units		(* tk option: -arrowshape {<units> <units> <units>} *)
	| ArrowStyle of arrowStyle		(* tk option: -arrow <arrowStyle> *)
	| Aspect of int		(* tk option: -aspect <int> *)
	| Background of color		(* tk option: -background <color> *)
	| Before of widget		(* tk option: -before <widget> *)
	| BgStipple of bitmap		(* tk option: -bgstipple <bitmap> *)
	| BigIncrement of float		(* tk option: -bigincrement <float> *)
	| Bitmap of bitmap		(* tk option: -bitmap <bitmap> *)
	| BorderMode of borderMode		(* tk option: -bordermode <borderMode> *)
	| BorderWidth of units		(* tk option: -borderwidth <units> *)
	| CapStyle of capStyle		(* tk option: -capstyle <capStyle> *)
	| Class of string		(* tk option: -class <string> *)
	| CloseEnough of float		(* tk option: -closeenough <float> *)
	| Colormap of colormap		(* tk option: -colormap <colormap> *)
	| Colormode of colorMode		(* tk option: -colormode <colorMode> *)
	| Column of int		(* tk option: -column <int> *)
	| ColumnSpan of int		(* tk option: -columnspan <int> *)
	| Command of (unit -> unit)		(* tk option: -command <(unit -> unit)> *)
	| Confine of bool		(* tk option: -confine <bool> *)
	| Cursor of cursor		(* tk option: -cursor <cursor> *)
	| Data of string		(* tk option: -data <string> *)
	| Digits of int		(* tk option: -digits <int> *)
	| DisabledForeground of color		(* tk option: -disabledforeground <color> *)
	| ElementBorderWidth of units		(* tk option: -elementborderwidth <units> *)
	| Expand of bool		(* tk option: -expand <bool> *)
	| ExportSelection of bool		(* tk option: -exportselection <bool> *)
	| Extent of float		(* tk option: -extent <float> *)
	| FgStipple of bitmap		(* tk option: -fgstipple <bitmap> *)
	| File of string		(* tk option: -file <string> *)
	| Fill of fillMode		(* tk option: -fill <fillMode> *)
	| FillColor of color		(* tk option: -fill <color> *)
	| Font of string		(* tk option: -font <string> *)
	| Foreground of color		(* tk option: -foreground <color> *)
	| Format of string		(* tk option: -format <string> *)
	| From of float		(* tk option: -from <float> *)
	| Gamma of float		(* tk option: -gamma <float> *)
	| Geometry of string		(* tk option: -geometry <string> *)
	| Height of units		(* tk option: -height <units> *)
	| HighlightBackground of color		(* tk option: -highlightbackground <color> *)
	| HighlightColor of color		(* tk option: -highlightcolor <color> *)
	| HighlightThickness of units		(* tk option: -highlightthickness <units> *)
	| IPadX of units		(* tk option: -ipadx <units> *)
	| IPadY of units		(* tk option: -ipady <units> *)
	| ImageBitmap of imageBitmap		(* tk option: -image <imageBitmap> *)
	| ImagePhoto of imagePhoto		(* tk option: -image <imagePhoto> *)
	| In of widget		(* tk option: -in <widget> *)
	| IndicatorOn of bool		(* tk option: -indicatoron <bool> *)
	| InsertBackground of color		(* tk option: -insertbackground <color> *)
	| InsertBorderWidth of units		(* tk option: -insertborderwidth <units> *)
	| InsertOffTime of int		(* tk option: -insertofftime <int> *)
	| InsertOnTime of int		(* tk option: -insertontime <int> *)
	| InsertWidth of units		(* tk option: -insertwidth <units> *)
	| JoinStyle of joinStyle		(* tk option: -joinstyle <joinStyle> *)
	| Jump of bool		(* tk option: -jump <bool> *)
	| Justify of justification		(* tk option: -justify <justification> *)
	| LMargin1 of units		(* tk option: -lmargin1 <units> *)
	| LMargin2 of units		(* tk option: -lmargin2 <units> *)
	| Label of string		(* tk option: -label <string> *)
	| Length of units		(* tk option: -length <units> *)
	| Maskdata of string		(* tk option: -maskdata <string> *)
	| Maskfile of string		(* tk option: -maskfile <string> *)
	| Menu of widget		(* tk option: -menu <widget> *)
	| OffValue of string		(* tk option: -offvalue <string> *)
	| Offset of units		(* tk option: -offset <units> *)
	| OnValue of string		(* tk option: -onvalue <string> *)
	| Orient of orientation		(* tk option: -orient <orientation> *)
	| Outline of color		(* tk option: -outline <color> *)
	| OutlineStipple of bitmap		(* tk option: -outlinestipple <bitmap> *)
	| OverStrike of bool		(* tk option: -overstrike <bool> *)
	| PadX of units		(* tk option: -padx <units> *)
	| PadY of units		(* tk option: -pady <units> *)
	| PageAnchor of anchor		(* tk option: -pageanchor <anchor> *)
	| PageHeight of units		(* tk option: -pageheight <units> *)
	| PageWidth of units		(* tk option: -pagewidth <units> *)
	| PageX of units		(* tk option: -pagex <units> *)
	| PageY of units		(* tk option: -pagey <units> *)
	| Palette of paletteType		(* tk option: -palette <paletteType> *)
	| PostCommand of (unit -> unit)		(* tk option: -postcommand <(unit -> unit)> *)
	| RMargin of units		(* tk option: -rmargin <units> *)
	| RelHeight of float		(* tk option: -relheight <float> *)
	| RelWidth of float		(* tk option: -relwidth <float> *)
	| RelX of float		(* tk option: -relx <float> *)
	| RelY of float		(* tk option: -rely <float> *)
	| Relief of relief		(* tk option: -relief <relief> *)
	| RepeatDelay of int		(* tk option: -repeatdelay <int> *)
	| RepeatInterval of int		(* tk option: -repeatinterval <int> *)
	| Resolution of float		(* tk option: -resolution <float> *)
	| Rotate of bool		(* tk option: -rotate <bool> *)
	| Row of int		(* tk option: -row <int> *)
	| RowSpan of int		(* tk option: -rowspan <int> *)
	| ScaleCommand of (float -> unit)		(* tk option: -command <(float -> unit)> *)
	| Screen of string		(* tk option: -screen <string> *)
	| ScrollCommand of (scrollValue -> unit)		(* tk option: -command <(scrollValue -> unit)> *)
	| ScrollRegion of units * units * units * units		(* tk option: -scrollregion {<units> <units> <units> <units>} *)
	| SelectBackground of color		(* tk option: -selectbackground <color> *)
	| SelectBorderWidth of units		(* tk option: -selectborderwidth <units> *)
	| SelectColor of color		(* tk option: -selectcolor <color> *)
	| SelectForeground of color		(* tk option: -selectforeground <color> *)
	| SelectImageBitmap of imageBitmap		(* tk option: -selectimage <imageBitmap> *)
	| SelectImagePhoto of imagePhoto		(* tk option: -selectimage <imagePhoto> *)
	| SelectMode of selectModeType		(* tk option: -selectmode <selectModeType> *)
	| SetGrid of bool		(* tk option: -setgrid <bool> *)
	| Show of char		(* tk option: -show <char> *)
	| ShowValue of bool		(* tk option: -showvalue <bool> *)
	| Side of side		(* tk option: -side <side> *)
	| SliderLength of units		(* tk option: -sliderlength <units> *)
	| Smooth of bool		(* tk option: -smooth <bool> *)
	| Spacing1 of units		(* tk option: -spacing1 <units> *)
	| Spacing2 of units		(* tk option: -spacing2 <units> *)
	| Spacing3 of units		(* tk option: -spacing3 <units> *)
	| SplineSteps of int		(* tk option: -splinesteps <int> *)
	| Start of float		(* tk option: -start <float> *)
	| State of state		(* tk option: -state <state> *)
	| Sticky of string		(* tk option: -sticky <string> *)
	| Stipple of bitmap		(* tk option: -stipple <bitmap> *)
	| Stretch of bool		(* tk option: -stretch <bool> *)
	| Tabs of tabType list		(* tk option: -tabs {<tabType list>} *)
	| Tags of tagOrId list		(* tk option: -tags {<tagOrId list>} *)
	| TakeFocus of bool		(* tk option: -takefocus <bool> *)
	| TearOff of bool		(* tk option: -tearoff <bool> *)
	| Text of string		(* tk option: -text <string> *)
	| TextHeight of int		(* tk option: -height <int> *)
	| TextVariable of textVariable		(* tk option: -textvariable <textVariable> *)
	| TextWidth of int		(* tk option: -width <int> *)
	| TickInterval of float		(* tk option: -tickinterval <float> *)
	| To of float		(* tk option: -to <float> *)
	| TroughColor of color		(* tk option: -troughcolor <color> *)
	| Underline of bool		(* tk option: -underline <bool> *)
	| UnderlinedChar of int		(* tk option: -underline <int> *)
	| Value of string		(* tk option: -value <string> *)
	| Variable of textVariable		(* tk option: -variable <textVariable> *)
	| Visual of visual		(* tk option: -visual <visual> *)
	| Width of units		(* tk option: -width <units> *)
	| Window of widget		(* tk option: -window <widget> *)
	| Wrap of wrapMode		(* tk option: -wrap <wrapMode> *)
	| WrapLength of units		(* tk option: -wraplength <units> *)
	| X of units		(* tk option: -x <units> *)
	| XScrollCommand of (float -> float -> unit)		(* tk option: -xscrollcommand <(float -> float -> unit)> *)
	| XScrollIncrement of units		(* tk option: -xscrollincrement <units> *)
	| Y of units		(* tk option: -y <units> *)
	| YScrollCommand of (float -> float -> unit)		(* tk option: -yscrollcommand <(float -> float -> unit)> *)
	| YScrollIncrement of units		(* tk option: -yscrollincrement <units> *)
type options_constrs =
	CAccelerator
	| CActiveBackground
	| CActiveBorderWidth
	| CActiveForeground
	| CActiveRelief
	| CAfter
	| CAlign
	| CAnchor
	| CArcStyle
	| CArrowShape
	| CArrowStyle
	| CAspect
	| CBackground
	| CBefore
	| CBgStipple
	| CBigIncrement
	| CBitmap
	| CBorderMode
	| CBorderWidth
	| CCapStyle
	| CClass
	| CCloseEnough
	| CColormap
	| CColormode
	| CColumn
	| CColumnSpan
	| CCommand
	| CConfine
	| CCursor
	| CData
	| CDigits
	| CDisabledForeground
	| CElementBorderWidth
	| CExpand
	| CExportSelection
	| CExtent
	| CFgStipple
	| CFile
	| CFill
	| CFillColor
	| CFont
	| CForeground
	| CFormat
	| CFrom
	| CGamma
	| CGeometry
	| CHeight
	| CHighlightBackground
	| CHighlightColor
	| CHighlightThickness
	| CIPadX
	| CIPadY
	| CImageBitmap
	| CImagePhoto
	| CIn
	| CIndicatorOn
	| CInsertBackground
	| CInsertBorderWidth
	| CInsertOffTime
	| CInsertOnTime
	| CInsertWidth
	| CJoinStyle
	| CJump
	| CJustify
	| CLMargin1
	| CLMargin2
	| CLabel
	| CLength
	| CMaskdata
	| CMaskfile
	| CMenu
	| COffValue
	| COffset
	| COnValue
	| COrient
	| COutline
	| COutlineStipple
	| COverStrike
	| CPadX
	| CPadY
	| CPageAnchor
	| CPageHeight
	| CPageWidth
	| CPageX
	| CPageY
	| CPalette
	| CPostCommand
	| CRMargin
	| CRelHeight
	| CRelWidth
	| CRelX
	| CRelY
	| CRelief
	| CRepeatDelay
	| CRepeatInterval
	| CResolution
	| CRotate
	| CRow
	| CRowSpan
	| CScaleCommand
	| CScreen
	| CScrollCommand
	| CScrollRegion
	| CSelectBackground
	| CSelectBorderWidth
	| CSelectColor
	| CSelectForeground
	| CSelectImageBitmap
	| CSelectImagePhoto
	| CSelectMode
	| CSetGrid
	| CShow
	| CShowValue
	| CSide
	| CSliderLength
	| CSmooth
	| CSpacing1
	| CSpacing2
	| CSpacing3
	| CSplineSteps
	| CStart
	| CState
	| CSticky
	| CStipple
	| CStretch
	| CTabs
	| CTags
	| CTakeFocus
	| CTearOff
	| CText
	| CTextHeight
	| CTextVariable
	| CTextWidth
	| CTickInterval
	| CTo
	| CTroughColor
	| CUnderline
	| CUnderlinedChar
	| CValue
	| CVariable
	| CVisual
	| CWidth
	| CWindow
	| CWrap
	| CWrapLength
	| CX
	| CXScrollCommand
	| CXScrollIncrement
	| CY
	| CYScrollCommand
	| CYScrollIncrement
type searchSpec =
	Above of tagOrId		(* tk option: above <tagOrId> *)
	| All		(* tk option: all *)
	| Below of tagOrId		(* tk option: below <tagOrId> *)
	| Closest of units * units		(* tk option: closest <units> <units> *)
	| ClosestHalo of units * units * units		(* tk option: closest <units> <units> <units> *)
	| ClosestHaloStart of units * units * units * tagOrId		(* tk option: closest <units> <units> <units> <tagOrId> *)
	| Enclosed of units * units * units * units		(* tk option: enclosed <units> <units> <units> <units> *)
	| Overlapping of int * int * int * int		(* tk option: overlapping <int> <int> <int> <int> *)
	| Withtag of tagOrId		(* tk option: withtag <tagOrId> *)
type canvasItem =
	Arc_item		(* tk option: arc *)
	| Bitmap_item		(* tk option: bitmap *)
	| Image_item		(* tk option: image *)
	| Line_item		(* tk option: line *)
	| Oval_item		(* tk option: oval *)
	| Polygon_item		(* tk option: polygon *)
	| Rectangle_item		(* tk option: rectangle *)
	| Text_item		(* tk option: text *)
	| User_item of string		(* tk option: <string> *)
	| Window_item		(* tk option: window *)
type icccm =
	DisplayOf of widget		(* tk option: -displayof <widget> *)
	| ICCCMFormat of string		(* tk option: -format <string> *)
	| ICCCMType of string		(* tk option: -type <string> *)
	| LostCommand of (unit -> unit)		(* tk option: -command <(unit -> unit)> *)
	| Selection of string		(* tk option: -selection <string> *)
type grabStatus =
	GrabGlobal		(* tk option: global *)
	| GrabLocal		(* tk option: local *)
	| GrabNone		(* tk option: none *)
type gridopt =
	Minsize of units		(* tk option: -minsize <units> *)
	| Weight of float		(* tk option: -weight <float> *)
type menuItem =
	Cascade_Item		(* tk option: cascade *)
	| Checkbutton_Item		(* tk option: checkbutton *)
	| Command_Item		(* tk option: command *)
	| Radiobutton_Item		(* tk option: radiobutton *)
	| Separator_Item		(* tk option: separator *)
	| TearOff_Item		(* tk option: tearoff *)
type optionPriority =
	Interactive		(* tk option: interactive *)
	| Priority of int		(* tk option: <int> *)
	| StartupFile		(* tk option: startupFile *)
	| UserDefault		(* tk option: userDefault *)
	| WidgetDefault		(* tk option: widgetDefault *)
type photo =
	ImgFormat of string		(* tk option: -format <string> *)
	| ImgFrom of int * int * int * int		(* tk option: -from <int> <int> <int> <int> *)
	| ImgTo of int * int * int * int		(* tk option: -to <int> <int> <int> <int> *)
	| Shrink		(* tk option: -shrink *)
	| Subsample of int * int		(* tk option: -subsample <int> <int> *)
	| TopLeft of int * int		(* tk option: -to <int> <int> *)
	| Zoom of int * int		(* tk option: -zoom <int> <int> *)
type widgetElement =
	Arrow1		(* tk option: arrow1 *)
	| Arrow2		(* tk option: arrow2 *)
	| Beyond		(* tk option:  *)
	| Slider		(* tk option: slider *)
	| Trough1		(* tk option: trough1 *)
	| Trough2		(* tk option: trough2 *)
type comparison =
	EQ		(* tk option: == *)
	| GE		(* tk option: >= *)
	| GT		(* tk option: > *)
	| LE		(* tk option: <= *)
	| LT		(* tk option: < *)
	| NEQ		(* tk option: != *)
type markDirection =
	Mark_Left		(* tk option: left *)
	| Mark_Right		(* tk option: right *)
type textSearch =
	Backwards		(* tk option: -backwards *)
	| Count of textVariable		(* tk option: -count <textVariable> *)
	| Exact		(* tk option: -exact *)
	| Forwards		(* tk option: -forwards *)
	| Nocase		(* tk option: -nocase *)
	| Regexp		(* tk option: -regexp *)
type atomId =
	AtomId of int		(* tk option: <int> *)
type focusModel =
	FocusActive		(* tk option: active *)
	| FocusPassive		(* tk option: passive *)
type wmFrom =
	FromProgram		(* tk option: program *)
	| FromUser		(* tk option: user *)
 val bind: 
    widget -> (modifier list * xEvent) list -> bindAction -> unit
  val break : unit -> unit
val appname_get : unit ->string 
(* tk invocation: tk appname *)

val appname_set : string -> unit 
(* tk invocation: tk appname <string> *)

val bindtags : widget -> bindings list -> unit 
(* tk invocation: bindtags <widget> {<bindings list>} *)

val bindtags_get : widget -> bindings list 
(* tk invocation: bindtags <widget> *)

val cget : widget -> options_constrs -> string 
(* tk invocation: <widget> cget <options_constrs> *)

val cgets : widget -> string -> string 
(* tk invocation: <widget> cget <string> *)

val destroy : widget -> unit 
(* tk invocation: destroy <widget> *)

val grid : widget list -> options list -> unit 
(* tk invocation: grid <widget list> <options list> *)

val lower_window : widget -> unit 
(* tk invocation: lower <widget> *)

val lower_window_below : widget -> widget -> unit 
(* tk invocation: lower <widget> <widget> *)

val pack : widget list -> options list -> unit 
(* tk invocation: pack <widget list> <options list> *)

val place : widget -> options list -> unit 
(* tk invocation: place <widget> <options list> *)

val raise_window : widget -> unit 
(* tk invocation: raise <widget> *)

val raise_window_above : widget -> widget -> unit 
(* tk invocation: raise <widget> <widget> *)

val update : unit ->unit 
(* tk invocation: update *)

val update_idletasks : unit ->unit 
(* tk invocation: update idletasks *)

end
module Place : sig
(* The place commands  *)
open Tk
open Widget
open Textvariable
val configure : widget -> options list -> unit 
(* tk invocation: place configure <widget> <options list> *)

val forget : widget -> unit 
(* tk invocation: place forget <widget> *)

val info : widget -> string 
(* tk invocation: place info <widget> *)

val slaves : widget -> widget list 
(* tk invocation: place slaves <widget> *)

end

module Resource : sig
(* The resource commands  *)
open Tk
open Widget
open Textvariable

val clear : unit ->unit 
(* tk invocation: option clear *)

val get : widget -> string -> string -> string 
(* tk invocation: option get <widget> <string> <string> *)


end

module Wm : sig
(* The wm commands  *)
open Tk
open Widget
open Textvariable
val aspect_get : widget -> int * int * int * int 
(* tk invocation: wm aspect <widget> *)

val aspect_set : widget -> int -> int -> int -> int -> unit 
(* tk invocation: wm aspect <widget> <int> <int> <int> <int> *)

val client_get : widget -> string 
(* tk invocation: wm client <widget> *)

val client_set : widget -> string -> unit 
(* tk invocation: wm client <widget> <string> *)


val colormapwindows_set : widget -> widget list -> unit 
(* tk invocation: wm colormapwindows <widget> {<widget list>} *)

val command_clear : widget -> unit 
(* tk invocation: wm command <widget>  *)

val command_get : widget -> string list 
(* tk invocation: wm command <widget> *)

val command_set : widget -> string list -> unit 
(* tk invocation: wm command <widget> {<string list>} *)

val deiconify : widget -> unit 
(* tk invocation: wm deiconify <widget> *)

val focusmodel_get : widget -> focusModel 
(* tk invocation: wm focusmodel <widget> *)

val focusmodel_set : widget -> focusModel -> unit 
(* tk invocation: wm focusmodel <widget> <focusModel> *)

val frame : widget -> string 
(* tk invocation: wm frame <widget> *)

val geometry_get : widget -> string 
(* tk invocation: wm geometry <widget> *)

val geometry_set : widget -> string -> unit 
(* tk invocation: wm geometry <widget> <string> *)

val grid_clear : widget -> unit 
(* tk invocation: wm grid <widget>     *)

val grid_get : widget -> int * int * int * int 
(* tk invocation: wm grid <widget> *)

val grid_set : widget -> int -> int -> int -> int -> unit 
(* tk invocation: wm grid <widget> <int> <int> <int> <int> *)

val group_clear : widget -> unit 
(* tk invocation: wm group <widget>  *)


val group_set : widget -> widget -> unit 
(* tk invocation: wm group <widget> <widget> *)

val iconbitmap_clear : widget -> unit 
(* tk invocation: wm iconbitmap <widget>  *)

val iconbitmap_get : widget -> bitmap 
(* tk invocation: wm iconbitmap <widget> *)

val iconbitmap_set : widget -> bitmap -> unit 
(* tk invocation: wm iconbitmap <widget> <bitmap> *)

val iconify : widget -> unit 
(* tk invocation: wm iconify <widget> *)

val iconmask_clear : widget -> unit 
(* tk invocation: wm iconmask <widget>  *)

val iconmask_get : widget -> bitmap 
(* tk invocation: wm iconmask <widget> *)

val iconmask_set : widget -> bitmap -> unit 
(* tk invocation: wm iconmask <widget> <bitmap> *)

val iconname_get : widget -> string 
(* tk invocation: wm iconname <widget> *)

val iconname_set : widget -> string -> unit 
(* tk invocation: wm iconname <widget> <string> *)

val iconposition_clear : widget -> unit 
(* tk invocation: wm iconposition <widget>   *)

val iconposition_get : widget -> int * int 
(* tk invocation: wm iconposition <widget> *)

val iconposition_set : widget -> int -> int -> unit 
(* tk invocation: wm iconposition <widget> <int> <int> *)

val iconwindow_clear : widget -> unit 
(* tk invocation: wm iconwindow <widget>  *)


val iconwindow_set : widget -> widget -> unit 
(* tk invocation: wm iconwindow <widget> <widget> *)

val maxsize_get : widget -> int * int 
(* tk invocation: wm maxsize <widget> *)

val maxsize_set : widget -> int -> int -> unit 
(* tk invocation: wm maxsize <widget> <int> <int> *)

val minsize_get : widget -> int * int 
(* tk invocation: wm minsize <widget> *)

val minsize_set : widget -> int -> int -> unit 
(* tk invocation: wm minsize <widget> <int> <int> *)

val overrideredirect_get : widget -> bool 
(* tk invocation: wm overrideredirect <widget> *)

val overrideredirect_set : widget -> bool -> unit 
(* tk invocation: wm overrideredirect <widget> <bool> *)

val positionfrom_clear : widget -> unit 
(* tk invocation: wm positionfrom <widget>  *)

val positionfrom_get : widget -> wmFrom 
(* tk invocation: wm positionfrom <widget> *)

val positionfrom_set : widget -> wmFrom -> unit 
(* tk invocation: wm positionfrom <widget> <wmFrom> *)

val protocol_clear : widget -> string -> unit 
(* tk invocation: wm protocol <widget> <string>  *)

val protocol_set : widget -> string -> (unit -> unit) -> unit 
(* tk invocation: wm protocol <widget> <string> <(unit -> unit)> *)

val protocols : widget -> string list 
(* tk invocation: wm protocol <widget> *)

val resizable_get : widget -> bool * bool 
(* tk invocation: wm resizable <widget> *)

val resizable_set : widget -> bool -> bool -> unit 
(* tk invocation: wm resizable <widget> <bool> <bool> *)

val sizefrom_clear : widget -> unit 
(* tk invocation: wm sizefrom <widget>  *)

val sizefrom_get : widget -> wmFrom 
(* tk invocation: wm sizefrom <widget> *)

val sizefrom_set : widget -> wmFrom -> unit 
(* tk invocation: wm sizefrom <widget> <wmFrom> *)

val state : widget -> string 
(* tk invocation: wm state <widget> *)

val title_get : widget -> string 
(* tk invocation: wm title <widget> *)

val title_set : widget -> string -> unit 
(* tk invocation: wm title <widget> <string> *)

val transient_clear : widget -> unit 
(* tk invocation: wm transient <widget>  *)


val transient_set : widget -> widget -> unit 
(* tk invocation: wm transient <widget> <widget> *)

val withdraw : widget -> unit 
(* tk invocation: wm withdraw <widget> *)

end

module Imagephoto : sig
(* The imagephoto commands  *)
open Tk
open Widget
open Textvariable
val blank : imagePhoto -> unit 
(* tk invocation: <imagePhoto> blank *)

val configure : imagePhoto -> options list -> unit 
(* tk invocation: <imagePhoto> configure <options list> *)

val configure_get : imagePhoto -> string 
(* tk invocation: <imagePhoto> configure *)

val copy : imagePhoto -> imagePhoto -> photo list -> unit 
(* tk invocation: <imagePhoto> copy <imagePhoto> <photo list> *)

val create : options list -> imagePhoto 
(* tk invocation: image create photo <options list> *)

val delete : imagePhoto -> unit 
(* tk invocation: image delete <imagePhoto> *)

val get : imagePhoto -> int -> int -> int * int * int 
(* tk invocation: <imagePhoto> get <int> <int> *)

val height : imagePhoto -> int 
(* tk invocation: image height <imagePhoto> *)

val read : imagePhoto -> string -> photo list -> unit 
(* tk invocation: <imagePhoto> read <string> <photo list> *)

val redither : imagePhoto -> unit 
(* tk invocation: <imagePhoto> redither *)

val width : imagePhoto -> int 
(* tk invocation: image width <imagePhoto> *)

val write : imagePhoto -> photo list -> unit 
(* tk invocation: <imagePhoto> write <photo list> *)

end

module Canvas : sig
(* The canvas widget *)
open Tk
open Widget
open Textvariable
val create : widget -> options list -> widget 
             (* [create p options] creates a new widget with parent p.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val create_named : widget -> string -> options list -> widget 
             (* [create p name options] creates a new widget with
                parent p and new patch component name.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val addtag : widget -> tagOrId -> searchSpec list -> unit 
(* tk invocation: <widget> addtag <tagOrId> <searchSpec list> *)

val bbox : widget -> tagOrId list -> int * int * int * int 
(* tk invocation: <widget> bbox <tagOrId list> *)

val canvasx : widget -> units -> float 
(* tk invocation: <widget> canvasx <units> *)

val canvasx_grid : widget -> units -> units -> float 
(* tk invocation: <widget> canvasx <units> <units> *)

val canvasy : widget -> units -> float 
(* tk invocation: <widget> canvasy <units> *)

val canvasy_grid : widget -> units -> units -> float 
(* tk invocation: <widget> canvasy <units> <units> *)

val configure : widget -> options list -> unit 
(* tk invocation: <widget> configure <options list> *)

val configure_arc : widget -> tagOrId -> options list -> unit 
(* tk invocation: <widget> itemconfigure <tagOrId> <options list> *)

val configure_bitmap : widget -> tagOrId -> options list -> unit 
(* tk invocation: <widget> itemconfigure <tagOrId> <options list> *)

val configure_get : widget -> string 
(* tk invocation: <widget> configure *)

val configure_image : widget -> tagOrId -> options list -> unit 
(* tk invocation: <widget> itemconfigure <tagOrId> <options list> *)

val configure_line : widget -> tagOrId -> options list -> unit 
(* tk invocation: <widget> itemconfigure <tagOrId> <options list> *)

val configure_oval : widget -> tagOrId -> options list -> unit 
(* tk invocation: <widget> itemconfigure <tagOrId> <options list> *)

val configure_polygon : widget -> tagOrId -> options list -> unit 
(* tk invocation: <widget> itemconfigure <tagOrId> <options list> *)

val configure_rectangle : widget -> tagOrId -> options list -> unit 
(* tk invocation: <widget> itemconfigure <tagOrId> <options list> *)

val configure_text : widget -> tagOrId -> options list -> unit 
(* tk invocation: <widget> itemconfigure <tagOrId> <options list> *)

val configure_window : widget -> tagOrId -> options list -> unit 
(* tk invocation: <widget> itemconfigure <tagOrId> <options list> *)

val coords_get : widget -> tagOrId -> float list 
(* tk invocation: <widget> coords <tagOrId> *)

val coords_set : widget -> tagOrId -> units list -> unit 
(* tk invocation: <widget> coords <tagOrId> <units list> *)

val create_arc : widget -> units -> units -> units -> units -> options list -> tagOrId 
(* tk invocation: <widget> create arc <units> <units> <units> <units> <options list> *)

val create_bitmap : widget -> units -> units -> options list -> tagOrId 
(* tk invocation: <widget> create bitmap <units> <units> <options list> *)

val create_image : widget -> units -> units -> options list -> tagOrId 
(* tk invocation: <widget> create image <units> <units> <options list> *)

val create_line : widget -> units list -> options list -> tagOrId 
(* tk invocation: <widget> create line <units list> <options list> *)

val create_oval : widget -> units -> units -> units -> units -> options list -> tagOrId 
(* tk invocation: <widget> create oval <units> <units> <units> <units> <options list> *)

val create_polygon : widget -> units list -> options list -> tagOrId 
(* tk invocation: <widget> create polygon <units list> <options list> *)

val create_rectangle : widget -> units -> units -> units -> units -> options list -> tagOrId 
(* tk invocation: <widget> create rectangle <units> <units> <units> <units> <options list> *)

val create_text : widget -> units -> units -> options list -> tagOrId 
(* tk invocation: <widget> create text <units> <units> <options list> *)

val create_window : widget -> units -> units -> options list -> tagOrId 
(* tk invocation: <widget> create window <units> <units> <options list> *)

val dchars : widget -> tagOrId -> index -> index -> unit 
(* tk invocation: <widget> dchars <tagOrId> <index> <index> *)

val delete : widget -> tagOrId list -> unit 
(* tk invocation: <widget> delete <tagOrId list> *)

val dtag : widget -> tagOrId -> tagOrId -> unit 
(* tk invocation: <widget> dtag <tagOrId> <tagOrId> *)

val find : widget -> searchSpec list -> tagOrId list 
(* tk invocation: <widget> find <searchSpec list> *)

val focus : widget -> tagOrId -> unit 
(* tk invocation: <widget> focus <tagOrId> *)

val focus_get : widget -> tagOrId 
(* tk invocation: <widget> focus *)

val focus_reset : widget -> unit 
(* tk invocation: <widget> focus  *)

val gettags : widget -> tagOrId -> tagOrId list 
(* tk invocation: <widget> gettags <tagOrId> *)

val icursor : widget -> tagOrId -> index -> unit 
(* tk invocation: <widget> icursor <tagOrId> <index> *)

val index : widget -> tagOrId -> index -> int 
(* tk invocation: <widget> index <tagOrId> <index> *)

val insert : widget -> tagOrId -> index -> string -> unit 
(* tk invocation: <widget> insert <tagOrId> <index> <string> *)

val itemconfigure_get : widget -> tagOrId -> string 
(* tk invocation: <widget> itemconfigure <tagOrId> *)

val lower_below : widget -> tagOrId -> tagOrId -> unit 
(* tk invocation: <widget> lower <tagOrId> <tagOrId> *)

val lower_bot : widget -> tagOrId -> unit 
(* tk invocation: <widget> lower <tagOrId> *)

val move : widget -> tagOrId -> units -> units -> unit 
(* tk invocation: <widget> move <tagOrId> <units> <units> *)


val raise_above : widget -> tagOrId -> tagOrId -> unit 
(* tk invocation: <widget> raise <tagOrId> <tagOrId> *)

val raise_top : widget -> tagOrId -> unit 
(* tk invocation: <widget> raise <tagOrId> *)

val scale : widget -> tagOrId -> units -> units -> float -> float -> unit 
(* tk invocation: <widget> scale <tagOrId> <units> <units> <float> <float> *)

val scan_dragto : widget -> int -> int -> unit 
(* tk invocation: <widget> scan dragto <int> <int> *)

val scan_mark : widget -> int -> int -> unit 
(* tk invocation: <widget> scan mark <int> <int> *)

val select_adjust : widget -> tagOrId -> index -> unit 
(* tk invocation: <widget> select adjust <tagOrId> <index> *)

val select_clear : widget -> unit 
(* tk invocation: <widget> select clear *)

val select_from : widget -> tagOrId -> index -> unit 
(* tk invocation: <widget> select from <tagOrId> <index> *)

val select_item : widget -> tagOrId 
(* tk invocation: <widget> select item *)

val select_to : widget -> tagOrId -> index -> unit 
(* tk invocation: <widget> select to <tagOrId> <index> *)

val typeof : widget -> tagOrId -> canvasItem 
(* tk invocation: <widget> type <tagOrId> *)

val xview : widget -> scrollValue -> unit 
(* tk invocation: <widget> xview <scrollValue> *)

val xview_get : widget -> float * float 
(* tk invocation: <widget> xview *)

val yview : widget -> scrollValue -> unit 
(* tk invocation: <widget> yview <scrollValue> *)

val yview_get : widget -> float * float 
(* tk invocation: <widget> yview *)

val bind : widget -> tagOrId -> 
                    (modifier list * xEvent) list -> bindAction -> unit 


end

module Button : sig
(* The button widget *)
open Tk
open Widget
open Textvariable
val create : widget -> options list -> widget 
             (* [create p options] creates a new widget with parent p.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val create_named : widget -> string -> options list -> widget 
             (* [create p name options] creates a new widget with
                parent p and new patch component name.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val configure : widget -> options list -> unit 
(* tk invocation: <widget> configure <options list> *)

val configure_get : widget -> string 
(* tk invocation: <widget> configure *)

val flash : widget -> unit 
(* tk invocation: <widget> flash *)

val invoke : widget -> unit 
(* tk invocation: <widget> invoke *)

end

module Text : sig
(* The text widget *)
open Tk
open Widget
open Textvariable
val create : widget -> options list -> widget 
             (* [create p options] creates a new widget with parent p.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val create_named : widget -> string -> options list -> widget 
             (* [create p name options] creates a new widget with
                parent p and new patch component name.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val bbox : widget -> textIndex -> int * int * int * int 
(* tk invocation: <widget> bbox <textIndex> *)

val compare : widget -> textIndex -> comparison -> textIndex -> bool 
(* tk invocation: <widget> compare <textIndex> <comparison> <textIndex> *)

val configure : widget -> options list -> unit 
(* tk invocation: <widget> configure <options list> *)

val configure_get : widget -> string 
(* tk invocation: <widget> configure *)

val debug : widget -> bool -> unit 
(* tk invocation: <widget> debug <bool> *)

val delete : widget -> textIndex -> textIndex -> unit 
(* tk invocation: <widget> delete <textIndex> <textIndex> *)

val delete_char : widget -> textIndex -> unit 
(* tk invocation: <widget> delete <textIndex> *)

val dlineinfo : widget -> textIndex -> int * int * int * int * int 
(* tk invocation: <widget> dlineinfo <textIndex> *)

val get : widget -> textIndex -> textIndex -> string 
(* tk invocation: <widget> get <textIndex> <textIndex> *)

val get_char : widget -> textIndex -> string 
(* tk invocation: <widget> get <textIndex> *)

val index : widget -> textIndex -> index 
(* tk invocation: <widget> index <textIndex> *)

val insert : widget -> textIndex -> string -> textTag list -> unit 
(* tk invocation: <widget> insert <textIndex> <string> {<textTag list>} *)

val mark_gravity_get : widget -> textMark -> markDirection 
(* tk invocation: <widget> mark gravity <textMark> *)

val mark_gravity_set : widget -> textMark -> markDirection -> unit 
(* tk invocation: <widget> mark gravity <textMark> <markDirection> *)

val mark_names : widget -> textMark list 
(* tk invocation: <widget> mark names *)

val mark_set : widget -> textMark -> textIndex -> unit 
(* tk invocation: <widget> mark set <textMark> <textIndex> *)

val mark_unset : widget -> textMark list -> unit 
(* tk invocation: <widget> mark unset <textMark list> *)

val scan_dragto : widget -> int -> int -> unit 
(* tk invocation: <widget> scan dragto <int> <int> *)

val scan_mark : widget -> int -> int -> unit 
(* tk invocation: <widget> scan mark <int> <int> *)

val search : widget -> textSearch list -> string -> textIndex -> textIndex -> index 
(* tk invocation: <widget> search <textSearch list> -- <string> <textIndex> <textIndex> *)

val see : widget -> textIndex -> unit 
(* tk invocation: <widget> see <textIndex> *)

val tag_add : widget -> textTag -> textIndex -> textIndex -> unit 
(* tk invocation: <widget> tag add <textTag> <textIndex> <textIndex> *)

val tag_add_char : widget -> textTag -> textIndex -> unit 
(* tk invocation: <widget> tag add <textTag> <textIndex> *)

val tag_allnames : widget -> textTag list 
(* tk invocation: <widget> tag names *)

val tag_configure : widget -> textTag -> options list -> unit 
(* tk invocation: <widget> tag configure <textTag> <options list> *)

val tag_delete : widget -> textTag list -> unit 
(* tk invocation: <widget> tag delete <textTag list> *)

val tag_indexnames : widget -> textIndex -> textTag list 
(* tk invocation: <widget> tag names <textIndex> *)

val tag_lower_below : widget -> textTag -> textTag -> unit 
(* tk invocation: <widget> tag lower <textTag> <textTag> *)

val tag_lower_bot : widget -> textTag -> unit 
(* tk invocation: <widget> tag lower <textTag> *)

val tag_nextrange : widget -> textTag -> textIndex -> textIndex -> index * index 
(* tk invocation: <widget> tag nextrange <textTag> <textIndex> <textIndex> *)

val tag_raise_above : widget -> textTag -> textTag -> unit 
(* tk invocation: <widget> tag raise <textTag> <textTag> *)

val tag_raise_top : widget -> textTag -> unit 
(* tk invocation: <widget> tag raise <textTag> *)

val tag_ranges : widget -> textTag -> index list 
(* tk invocation: <widget> tag ranges <textTag> *)

val tag_remove : widget -> textTag -> textIndex -> textIndex -> unit 
(* tk invocation: <widget> tag remove <textTag> <textIndex> <textIndex> *)

val tag_remove_char : widget -> textTag -> textIndex -> unit 
(* tk invocation: <widget> tag remove <textTag> <textIndex> *)

val window_configure : widget -> textTag -> options list -> unit 
(* tk invocation: <widget> window configure <textTag> <options list> *)

val window_create : widget -> textIndex -> options list -> unit 
(* tk invocation: <widget> window create <textIndex> <options list> *)

val window_names : widget -> widget list 
(* tk invocation: <widget> window names *)

val xview : widget -> scrollValue -> unit 
(* tk invocation: <widget> xview <scrollValue> *)

val xview_get : widget -> float * float 
(* tk invocation: <widget> xview *)

val yview : widget -> scrollValue -> unit 
(* tk invocation: <widget> yview <scrollValue> *)

val yview_get : widget -> float * float 
(* tk invocation: <widget> yview *)

val yview_index : widget -> textIndex -> unit 
(* tk invocation: <widget> yview <textIndex> *)

val yview_index_pickplace : widget -> textIndex -> unit 
(* tk invocation: <widget> yview -pickplace <textIndex> *)

val yview_line : widget -> int -> unit 
(* tk invocation: <widget> yview <int> *)

val tag_bind: widget -> textTag ->
                       (modifier list * xEvent) list -> bindAction -> unit 


end

module Label : sig
(* The label widget *)
open Tk
open Widget
open Textvariable
val create : widget -> options list -> widget 
             (* [create p options] creates a new widget with parent p.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val create_named : widget -> string -> options list -> widget 
             (* [create p name options] creates a new widget with
                parent p and new patch component name.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val configure : widget -> options list -> unit 
(* tk invocation: <widget> configure <options list> *)

val configure_get : widget -> string 
(* tk invocation: <widget> configure *)

end

module Scrollbar : sig
(* The scrollbar widget *)
open Tk
open Widget
open Textvariable
val create : widget -> options list -> widget 
             (* [create p options] creates a new widget with parent p.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val create_named : widget -> string -> options list -> widget 
             (* [create p name options] creates a new widget with
                parent p and new patch component name.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val activate : widget -> widgetElement -> unit 
(* tk invocation: <widget> activate <widgetElement> *)

val activate_get : widget -> widgetElement 
(* tk invocation: <widget> activate *)

val configure : widget -> options list -> unit 
(* tk invocation: <widget> configure <options list> *)

val configure_get : widget -> string 
(* tk invocation: <widget> configure *)

val delta : widget -> int -> int -> float 
(* tk invocation: <widget> delta <int> <int> *)

val fraction : widget -> int -> int -> float 
(* tk invocation: <widget> fraction <int> <int> *)

val get : widget -> float * float 
(* tk invocation: <widget> get *)

val identify : widget -> int -> int -> widgetElement 
(* tk invocation: <widget> identify <int> <int> *)

val old_get : widget -> int * int * int * int 
(* tk invocation: <widget> get *)

val old_set : widget -> int -> int -> int -> int -> unit 
(* tk invocation: <widget> set <int> <int> <int> <int> *)

val set : widget -> float -> float -> unit 
(* tk invocation: <widget> set <float> <float> *)

end

module Pixmap : sig
(* The pixmap commands  *)
open Tk
open Widget
open Textvariable
open Tk
(*
 * Minimal pixmap support
 *)

type t
type pixel

val width : t -> int
    (* [width pixmap] *)
val height : t -> int
    (* [height pixmap] *)

val create : int -> int -> t
    (* [create width height] *)
val get : imagePhoto -> t
    (* [get img] *)
val set : imagePhoto -> t -> unit
    (* [set img pixmap] *)
val blit : imagePhoto -> t -> int -> int -> int -> int -> unit
    (* [blit img pixmap x y w h] (all ints must be non-negative) *)
val from_file : string -> t
    (* [from_file filename] *)

val copy : t -> t -> unit
    (* [copy src dst] *)

(*
 * Pixel operations
 *)
val get_pixel : t -> int -> int -> pixel
    (* [get_pixel pixmap x y] *)
val set_pixel : t -> int -> int -> pixel -> unit
    (* [set_pixel pixmap x y pixel] *)
val default_color : pixel

val pixel : int -> int -> int -> pixel
    (* [pixel r g b]   (r,g,b must be in [0..255]) *)

(*-*)


end

module Palette : sig
(* The palette commands  *)
open Tk
open Widget
open Textvariable
val bisque : unit ->unit 
(* tk invocation: tk_bisque *)

val set_background : color -> unit 
(* tk invocation: tk_setPalette <color> *)

end

module Message : sig
(* The message widget *)
open Tk
open Widget
open Textvariable
val create : widget -> options list -> widget 
             (* [create p options] creates a new widget with parent p.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val create_named : widget -> string -> options list -> widget 
             (* [create p name options] creates a new widget with
                parent p and new patch component name.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val configure : widget -> options list -> unit 
(* tk invocation: <widget> configure <options list> *)

val configure_get : widget -> string 
(* tk invocation: <widget> configure *)

end

module Menu : sig
(* The menu widget *)
open Tk
open Widget
open Textvariable
val create : widget -> options list -> widget 
             (* [create p options] creates a new widget with parent p.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val create_named : widget -> string -> options list -> widget 
             (* [create p name options] creates a new widget with
                parent p and new patch component name.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val activate : widget -> index -> unit 
(* tk invocation: <widget> activate <index> *)

val add_cascade : widget -> options list -> unit 
(* tk invocation: <widget> add cascade <options list> *)

val add_checkbutton : widget -> options list -> unit 
(* tk invocation: <widget> add checkbutton <options list> *)

val add_command : widget -> options list -> unit 
(* tk invocation: <widget> add command <options list> *)

val add_radiobutton : widget -> options list -> unit 
(* tk invocation: <widget> add radiobutton <options list> *)

val add_separator : widget -> unit 
(* tk invocation: <widget> add separator *)

val configure : widget -> options list -> unit 
(* tk invocation: <widget> configure <options list> *)

val configure_cascade : widget -> index -> options list -> unit 
(* tk invocation: <widget> entryconfigure <index> <options list> *)

val configure_checkbutton : widget -> index -> options list -> unit 
(* tk invocation: <widget> entryconfigure <index> <options list> *)

val configure_command : widget -> index -> options list -> unit 
(* tk invocation: <widget> entryconfigure <index> <options list> *)

val configure_get : widget -> string 
(* tk invocation: <widget> configure *)

val configure_radiobutton : widget -> index -> options list -> unit 
(* tk invocation: <widget> entryconfigure <index> <options list> *)

val delete : widget -> index -> index -> unit 
(* tk invocation: <widget> delete <index> <index> *)

val entryconfigure_get : widget -> index -> string 
(* tk invocation: <widget> entryconfigure <index> *)

val index : widget -> index -> int 
(* tk invocation: <widget> index <index> *)

val insert_cascade : widget -> index -> options list -> unit 
(* tk invocation: <widget> insert <index> cascade <options list> *)

val insert_checkbutton : widget -> index -> options list -> unit 
(* tk invocation: <widget> insert <index> checkbutton <options list> *)

val insert_command : widget -> index -> options list -> unit 
(* tk invocation: <widget> insert <index> command <options list> *)

val insert_radiobutton : widget -> index -> options list -> unit 
(* tk invocation: <widget> insert <index> radiobutton <options list> *)

val insert_separator : widget -> index -> unit 
(* tk invocation: <widget> insert <index> separator *)

val invoke : widget -> index -> string 
(* tk invocation: <widget> invoke <index> *)

val popup : widget -> int -> int -> unit 
(* tk invocation: tk_popup <widget> <int> <int> *)

val popup_entry : widget -> int -> int -> index -> unit 
(* tk invocation: tk_popup <widget> <int> <int> <index> *)

val post : widget -> int -> int -> unit 
(* tk invocation: <widget> post <int> <int> *)

val postcascade : widget -> index -> unit 
(* tk invocation: <widget> postcascade <index> *)

val typeof : widget -> index -> menuItem 
(* tk invocation: <widget> type <index> *)

val unpost : widget -> unit 
(* tk invocation: <widget> unpost *)

val yposition : widget -> index -> int 
(* tk invocation: <widget> yposition <index> *)

end

module Entry : sig
(* The entry widget *)
open Tk
open Widget
open Textvariable
val create : widget -> options list -> widget 
             (* [create p options] creates a new widget with parent p.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val create_named : widget -> string -> options list -> widget 
             (* [create p name options] creates a new widget with
                parent p and new patch component name.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val configure : widget -> options list -> unit 
(* tk invocation: <widget> configure <options list> *)

val configure_get : widget -> string 
(* tk invocation: <widget> configure *)

val delete_range : widget -> index -> index -> unit 
(* tk invocation: <widget> delete <index> <index> *)

val delete_single : widget -> index -> unit 
(* tk invocation: <widget> delete <index> *)

val get : widget -> string 
(* tk invocation: <widget> get *)

val icursor : widget -> index -> unit 
(* tk invocation: <widget> icursor <index> *)

val index : widget -> index -> int 
(* tk invocation: <widget> index <index> *)

val insert : widget -> index -> string -> unit 
(* tk invocation: <widget> insert <index> <string> *)

val scan_dragto : widget -> int -> unit 
(* tk invocation: <widget> scan dragto <int> *)

val scan_mark : widget -> int -> unit 
(* tk invocation: <widget> scan mark <int> *)

val selection_adjust : widget -> index -> unit 
(* tk invocation: <widget> selection adjust <index> *)

val selection_clear : widget -> unit 
(* tk invocation: <widget> selection clear *)

val selection_from : widget -> index -> unit 
(* tk invocation: <widget> selection from <index> *)

val selection_present : widget -> bool 
(* tk invocation: <widget> selection present *)

val selection_range : widget -> index -> index -> unit 
(* tk invocation: <widget> selection range <index> <index> *)

val selection_to : widget -> index -> unit 
(* tk invocation: <widget> selection to <index> *)

val xview : widget -> scrollValue -> unit 
(* tk invocation: <widget> xview <scrollValue> *)

val xview_get : widget -> float * float 
(* tk invocation: <widget> xview *)

val xview_index : widget -> index -> unit 
(* tk invocation: <widget> xview <index> *)

end

module Listbox : sig
(* The listbox widget *)
open Tk
open Widget
open Textvariable
val create : widget -> options list -> widget 
             (* [create p options] creates a new widget with parent p.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val create_named : widget -> string -> options list -> widget 
             (* [create p name options] creates a new widget with
                parent p and new patch component name.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val activate : widget -> index -> unit 
(* tk invocation: <widget> activate <index> *)

val bbox : widget -> index -> int * int * int * int 
(* tk invocation: <widget> bbox <index> *)

val configure : widget -> options list -> unit 
(* tk invocation: <widget> configure <options list> *)

val configure_get : widget -> string 
(* tk invocation: <widget> configure *)

val curselection : widget -> index list 
(* tk invocation: <widget> curselection *)

val delete : widget -> index -> index -> unit 
(* tk invocation: <widget> delete <index> <index> *)

val get : widget -> index -> string 
(* tk invocation: <widget> get <index> *)

val get_range : widget -> index -> index -> string list 
(* tk invocation: <widget> get <index> <index> *)

val index : widget -> index -> int 
(* tk invocation: <widget> index <index> *)

val insert : widget -> index -> string list -> unit 
(* tk invocation: <widget> insert <index> <string list> *)

val nearest : widget -> int -> index 
(* tk invocation: <widget> nearest <int> *)

val scan_dragto : widget -> int -> int -> unit 
(* tk invocation: <widget> scan dragto <int> <int> *)

val scan_mark : widget -> int -> int -> unit 
(* tk invocation: <widget> scan mark <int> <int> *)

val see : widget -> index -> unit 
(* tk invocation: <widget> see <index> *)

val selection_anchor : widget -> index -> unit 
(* tk invocation: <widget> selection anchor <index> *)

val selection_clear : widget -> index -> index -> unit 
(* tk invocation: <widget> selection clear <index> <index> *)

val selection_includes : widget -> index -> bool 
(* tk invocation: <widget> selection includes <index> *)

val selection_set : widget -> index -> index -> unit 
(* tk invocation: <widget> selection set <index> <index> *)

val size : widget -> int 
(* tk invocation: <widget> size *)

val xview : widget -> scrollValue -> unit 
(* tk invocation: <widget> xview <scrollValue> *)

val xview_get : widget -> float * float 
(* tk invocation: <widget> xview *)

val xview_index : widget -> index -> unit 
(* tk invocation: <widget> xview <index> *)

val yview : widget -> scrollValue -> unit 
(* tk invocation: <widget> yview <scrollValue> *)

val yview_get : widget -> float * float 
(* tk invocation: <widget> yview *)

val yview_index : widget -> index -> unit 
(* tk invocation: <widget> yview <index> *)

end

module Focus : sig
(* The focus commands  *)
open Tk
open Widget
open Textvariable

val follows_mouse : unit ->unit 
(* tk invocation: tk_focusFollowsMouse *)

val force : widget -> unit 
(* tk invocation: focus -force <widget> *)





val set : widget -> unit 
(* tk invocation: focus <widget> *)

end

module Menubutton : sig
(* The menubutton widget *)
open Tk
open Widget
open Textvariable
val create : widget -> options list -> widget 
             (* [create p options] creates a new widget with parent p.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val create_named : widget -> string -> options list -> widget 
             (* [create p name options] creates a new widget with
                parent p and new patch component name.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val configure : widget -> options list -> unit 
(* tk invocation: <widget> configure <options list> *)

val configure_get : widget -> string 
(* tk invocation: <widget> configure *)

end

module Pack : sig
(* The pack commands  *)
open Tk
open Widget
open Textvariable
val configure : widget list -> options list -> unit 
(* tk invocation: pack configure <widget list> <options list> *)

val forget : widget list -> unit 
(* tk invocation: pack forget <widget list> *)

val propagate_get : widget -> bool 
(* tk invocation: pack propagate <widget> *)

val propagate_set : widget -> bool -> unit 
(* tk invocation: pack propagate <widget> <bool> *)

val slaves : widget -> widget list 
(* tk invocation: pack slaves <widget> *)

end

module Toplevel : sig
(* The toplevel widget *)
open Tk
open Widget
open Textvariable
val create : widget -> options list -> widget 
             (* [create p options] creates a new widget with parent p.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val create_named : widget -> string -> options list -> widget 
             (* [create p name options] creates a new widget with
                parent p and new patch component name.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val configure : widget -> options list -> unit 
(* tk invocation: <widget> configure <options list> *)

val configure_get : widget -> string 
(* tk invocation: <widget> configure *)

end

module Frame : sig
(* The frame widget *)
open Tk
open Widget
open Textvariable
val create : widget -> options list -> widget 
             (* [create p options] creates a new widget with parent p.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val create_named : widget -> string -> options list -> widget 
             (* [create p name options] creates a new widget with
                parent p and new patch component name.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val configure : widget -> options list -> unit 
(* tk invocation: <widget> configure <options list> *)

val configure_get : widget -> string 
(* tk invocation: <widget> configure *)

end

module Dialog : sig
(* The dialog commands  *)
open Tk
open Widget
open Textvariable
val create : 
  widget -> string -> string -> bitmap -> int -> string list -> int 
  (* [create parent title message bitmap default button_names] 
     cf. tk_dialog *)

val create_named :
  widget -> string -> string -> string -> bitmap -> int -> string list
       -> int 
  (* [create_named parent name title message bitmap default button_names] 
     cf. tk_dialog *)



end

module Imagebitmap : sig
(* The imagebitmap commands  *)
open Tk
open Widget
open Textvariable
val configure : imageBitmap -> options list -> unit 
(* tk invocation: <imageBitmap> configure <options list> *)

val configure_get : imageBitmap -> string 
(* tk invocation: <imageBitmap> configure *)

val create : options list -> imageBitmap 
(* tk invocation: image create bitmap <options list> *)

val create_named : imageBitmap -> options list -> imageBitmap 
(* tk invocation: image create bitmap <imageBitmap> <options list> *)

val delete : imageBitmap -> unit 
(* tk invocation: image delete <imageBitmap> *)

val height : imageBitmap -> int 
(* tk invocation: image height <imageBitmap> *)

val width : imageBitmap -> int 
(* tk invocation: image width <imageBitmap> *)

end

module Clipboard : sig
(* The clipboard commands  *)
open Tk
open Widget
open Textvariable
val append : icccm list -> string -> unit 
(* tk invocation: clipboard append <icccm list> -- <string> *)

val clear : icccm list -> unit 
(* tk invocation: clipboard clear <icccm list> *)

end

module Radiobutton : sig
(* The radiobutton widget *)
open Tk
open Widget
open Textvariable
val create : widget -> options list -> widget 
             (* [create p options] creates a new widget with parent p.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val create_named : widget -> string -> options list -> widget 
             (* [create p name options] creates a new widget with
                parent p and new patch component name.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val configure : widget -> options list -> unit 
(* tk invocation: <widget> configure <options list> *)

val configure_get : widget -> string 
(* tk invocation: <widget> configure *)

val deselect : widget -> unit 
(* tk invocation: <widget> deselect *)

val flash : widget -> unit 
(* tk invocation: <widget> flash *)

val invoke : widget -> unit 
(* tk invocation: <widget> invoke *)

val select : widget -> unit 
(* tk invocation: <widget> select *)

end

module Tkwait : sig
(* The tkwait commands  *)
open Tk
open Widget
open Textvariable
val variable : textVariable -> unit 
(* tk invocation: tkwait variable <textVariable> *)

val visibility : widget -> unit 
(* tk invocation: tkwait visibility <widget> *)

val window : widget -> unit 
(* tk invocation: tkwait window <widget> *)

end

module Grab : sig
(* The grab commands  *)
open Tk
open Widget
open Textvariable


val release : widget -> unit 
(* tk invocation: grab release <widget> *)

val set : widget -> unit 
(* tk invocation: grab <widget> *)

val set_global : widget -> unit 
(* tk invocation: grab -global <widget> *)

val status : widget -> grabStatus 
(* tk invocation: grab status <widget> *)

end

module Selection : sig
(* The selection commands  *)
open Tk
open Widget
open Textvariable
val clear : icccm list -> unit 
(* tk invocation: selection clear <icccm list> *)

val get : icccm list -> string 
(* tk invocation: selection get <icccm list> *)


val handle_set : icccm list -> widget -> (int -> int -> unit) -> unit
(* tk invocation: selection handle <icccm list> <widget> <command> *)


val own_set : icccm list -> widget -> unit 
(* tk invocation: selection own <icccm list> <widget> *)


end

module Scale : sig
(* The scale widget *)
open Tk
open Widget
open Textvariable
val create : widget -> options list -> widget 
             (* [create p options] creates a new widget with parent p.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val create_named : widget -> string -> options list -> widget 
             (* [create p name options] creates a new widget with
                parent p and new patch component name.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val configure : widget -> options list -> unit 
(* tk invocation: <widget> configure <options list> *)

val configure_get : widget -> string 
(* tk invocation: <widget> configure *)

val get : widget -> float 
(* tk invocation: <widget> get *)

val get_xy : widget -> int -> int -> float 
(* tk invocation: <widget> get <int> <int> *)

val identify : widget -> int -> int -> widgetElement 
(* tk invocation: <widget> <int> <int> *)

val set : widget -> float -> unit 
(* tk invocation: <widget> set <float> *)

end

module Optionmenu : sig
(* The optionmenu commands  *)
open Tk
open Widget
open Textvariable
(* Support for tk_optionMenu *)
val create: widget -> textVariable -> string list -> widget * widget
      	      (* [create parent var options] creates a multi-option 
      	       	 menubutton and its associated menu. The option is also stored
                 in the variable. Both widgets (menubutton and menu) are
		 returned *)


end

module Winfo : sig
(* The winfo commands  *)
open Tk
open Widget
open Textvariable




val cells : widget -> int 
(* tk invocation: winfo cells <widget> *)

val children : widget -> widget list 
(* tk invocation: winfo children <widget> *)

val class_name : widget -> string 
(* tk invocation: winfo class <widget> *)

val colormapfull : widget -> bool 
(* tk invocation: winfo colormapfull <widget> *)



val depth : widget -> int 
(* tk invocation: winfo depth <widget> *)

val exists : widget -> bool 
(* tk invocation: winfo exists <widget> *)

val fpixels : widget -> units -> float 
(* tk invocation: winfo fpixels <widget> <units> *)

val geometry : widget -> string 
(* tk invocation: winfo geometry <widget> *)

val height : widget -> int 
(* tk invocation: winfo height <widget> *)




val ismapped : widget -> bool 
(* tk invocation: winfo ismapped <widget> *)

val manager : widget -> string 
(* tk invocation: winfo manager <widget> *)

val name : widget -> string 
(* tk invocation: winfo name <widget> *)




val pixels : widget -> units -> int 
(* tk invocation: winfo pixels <widget> <units> *)

val pointerx : widget -> int 
(* tk invocation: winfo pointerx <widget> *)

val pointerxy : widget -> int * int 
(* tk invocation: winfo pointerxy <widget> *)

val pointery : widget -> int 
(* tk invocation: winfo pointery <widget> *)

val reqheight : widget -> int 
(* tk invocation: winfo reqheight <widget> *)

val reqwidth : widget -> int 
(* tk invocation: winfo reqwidth <widget> *)

val rgb : widget -> color -> int * int * int 
(* tk invocation: winfo rgb <widget> <color> *)

val rootx : widget -> int 
(* tk invocation: winfo rootx <widget> *)

val rooty : widget -> int 
(* tk invocation: winfo rooty <widget> *)

val screen : widget -> string 
(* tk invocation: winfo screen <widget> *)

val screencells : widget -> int 
(* tk invocation: winfo screencells <widget> *)

val screendepth : widget -> int 
(* tk invocation: winfo screendepth <widget> *)

val screenheight : widget -> int 
(* tk invocation: winfo screenheight <widget> *)

val screenmmdepth : widget -> int 
(* tk invocation: winfo screenmmdepth <widget> *)

val screenmmheight : widget -> int 
(* tk invocation: winfo screenmmheight <widget> *)

val screenvisual : widget -> string 
(* tk invocation: winfo screenvisual <widget> *)

val screenwidth : widget -> int 
(* tk invocation: winfo screenwidth <widget> *)



val viewable : widget -> bool 
(* tk invocation: winfo viewable <widget> *)

val visual : widget -> string 
(* tk invocation: winfo visual <widget> *)

val visualsavailable : widget -> string 
(* tk invocation: winfo visualsavailable <widget> *)

val vrootheight : widget -> int 
(* tk invocation: winfo vrootheight <widget> *)

val vrootwidth : widget -> int 
(* tk invocation: winfo vrootwidth <widget> *)

val vrootx : widget -> int 
(* tk invocation: winfo vrootx <widget> *)

val vrooty : widget -> int 
(* tk invocation: winfo vrooty <widget> *)

val width : widget -> int 
(* tk invocation: winfo width <widget> *)

val x : widget -> int 
(* tk invocation: winfo x <widget> *)

val y : widget -> int 
(* tk invocation: winfo y <widget> *)

val contained : int -> int -> widget -> bool
(* [contained x y w] returns true if (x,y) is in w *)


end

module Grid : sig
(* The grid commands  *)
open Tk
open Widget
open Textvariable
val all_slaves : widget -> widget list 
(* tk invocation: grid slaves <widget> *)

val bbox : widget -> int -> int -> int * int * int * int 
(* tk invocation: grid bbox <widget> <int> <int> *)

val column_configure : widget -> int -> gridopt list -> unit 
(* tk invocation: grid columnconfigure <widget> <int> <gridopt list> *)

val column_slaves : widget -> int -> widget list 
(* tk invocation: grid slaves <widget> -column <int> *)

val configure : widget list -> options list -> unit 
(* tk invocation: grid configure <widget list> <options list> *)

val forget : widget list -> unit 
(* tk invocation: grid forget <widget list> *)

val info : widget -> string 
(* tk invocation: grid info <widget> *)

val location : widget -> units -> units -> int * int 
(* tk invocation: grid location <widget> <units> <units> *)

val propagate_get : widget -> bool 
(* tk invocation: grid propagate <widget> *)

val propagate_set : widget -> bool -> unit 
(* tk invocation: grid propagate <widget> <bool> *)

val row_configure : widget -> int -> gridopt list -> unit 
(* tk invocation: grid rowconfigure <widget> <int> <gridopt list> *)

val row_slaves : widget -> int -> widget list 
(* tk invocation: grid slaves <widget> -row <int> *)

val size : widget -> int * int 
(* tk invocation: grid size <widget> *)

end

module Checkbutton : sig
(* The checkbutton widget *)
open Tk
open Widget
open Textvariable
val create : widget -> options list -> widget 
             (* [create p options] creates a new widget with parent p.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val create_named : widget -> string -> options list -> widget 
             (* [create p name options] creates a new widget with
                parent p and new patch component name.
                Options are restricted to the widget class subset,
                and checked dynamically. *)
val configure : widget -> options list -> unit 
(* tk invocation: <widget> configure <options list> *)

val configure_get : widget -> string 
(* tk invocation: <widget> configure *)

val deselect : widget -> unit 
(* tk invocation: <widget> deselect *)

val flash : widget -> unit 
(* tk invocation: <widget> flash *)

val invoke : widget -> unit 
(* tk invocation: <widget> invoke *)

val select : widget -> unit 
(* tk invocation: <widget> select *)

val toggle : widget -> unit 
(* tk invocation: <widget> toggle *)

end

module Bell : sig
(* The bell commands  *)
open Tk
open Widget
open Textvariable
val ring : unit ->unit 
(* tk invocation: bell *)

val ring_displayof : widget -> unit 
(* tk invocation: bell -displayof <widget> *)

end

module Frx_misc : sig
  val create_photo : Tk.options list -> Tk.imagePhoto
end

module Frx_req : sig
  val open_simple : 
    string -> (string -> unit) -> (unit -> unit) -> Textvariable.textVariable 
    -> unit
  val open_simple_synchronous : string -> Textvariable.textVariable -> bool
  val open_list :
    string -> string list -> (string -> unit) -> (unit -> unit) -> unit
  val open_passwd : string -> (string * string)
  end

module Frx_dialog: sig
  val f : 
   Widget.widget -> string -> string -> string -> Tk.bitmap -> int -> string list -> int
  end

module Jtk : sig
  open Tk
  open Widget

  type jtkOptions =
	KanjiFont of string

  type kanjiCode =
	  ANY		(* tk option: ANY *)
	| EUC		(* tk option: EUC *)
	| JIS		(* tk option: JIS *)
	| SJIS		(* tk option: SJIS *)

  module Kanji : sig
    val canvas_item : widget -> tagOrId -> jtkOptions list -> unit 
    val code : string -> kanjiCode 
    val conversion : kanjiCode -> kanjiCode -> string -> string 
    val internal_code_get : kanjiCode -> kanjiCode 
    val menu_entry : widget -> index -> jtkOptions list -> unit 
    val string_length : string -> int 
    val text_tag : widget -> textTag -> jtkOptions list -> unit 
    val widget_kanjifont : widget -> jtkOptions list -> unit 
  end
end
module Tkanim : sig
(*** Data types ***)

type animatedGif

    (* This data type contains all the information of an animation of
       gif89a format. It is still test implementation, so I should 
       keep it abstract. --- JPF *)

type imageType =
    Still of Tk.options
  | Animated of animatedGif

      (* This data type is required to distinguish normal still images
         and animated gifs. Usually objects typed imagePhoto or
	 imageBitmap are used for Still. *)

(*** Library availability check ***)

val available : unit -> bool

      (* [available ()] returns true if there is Tkanim Tcl/Tk
	 extension linked statically/dynamically in Tcl/Tk
	 interpreter. Otherwise, return false. *)

(*** User intaface ***)

(* create reads file... So, it is not safe. *)
(* val create  : string -> imageType *)

val delete  : animatedGif -> unit

      (* [delete anim] deletes all the images in anim. Usually
         animatedGifs contain many images, so you must not forget to
	 use this function to free the memory. *)

val width   : animatedGif -> int
val height  : animatedGif -> int

      (* [width anim] and [height anim] return the width and height of
	 given animated gif. *)

val images  : animatedGif -> Tk.imagePhoto list

      (* [images anim] returns the list of still images used in the 
	 animation *)

val animate : Widget.widget -> animatedGif -> bool -> unit
val animate_canvas_item : Widget.widget -> Tk.tagOrId -> animatedGif -> bool -> unit

      (*   They are the display function for animated gifs. Because
         animatedGif is abstracted, they are the only way to do it. 
	 [animate label anim] and [animate_canvas_item canvas tag anim]
	 display animations [anim] on a label widget [label] and an
	 image tag [tag] on a canvas widget [canvas] respectively.
	 Note that animation is stopped as default.
           These functions return interface functions, say, 
	 [inter : bool -> unit]. Currently, [inter false] toggles 
	 start/stop of the animation, and [inter true] displays the
	 next frame of the animation if it is stopped. *)

val gifdata : string -> imageType

      (* [gifdata data] reads [data] as a row data of a gif file and
	 decode it. You can use this funciton to embed images in your
	 applets. *)

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
val output : out_channel -> string -> int -> int -> unit
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
val input : in_channel -> string -> int -> int -> int
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
val really_input : in_channel -> string -> int -> int -> unit
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

module Str : sig
(* Module [Str]: regular expressions and high-level string processing *)

(*** Regular expressions *)

type regexp
        (* The type of compiled regular expressions. *)

val regexp: string -> regexp
        (* Compile a regular expression. The syntax for regular expressions
           is the same as in Gnu Emacs. The special characters are
           [$^.*+?[]\]. The following constructs are recognized:
-          [.     ] matches any character except newline
-          [*     ] (postfix) matches the previous expression zero, one or
                    several times
-          [+     ] (postfix) matches the previous expression one or
                    several times
-          [?     ] (postfix) matches the previous expression once or
                    not at all
-          [[..]  ] character set; ranges are denoted with [-], as in [a-z];
                    an initial [^], as in [^0-9], complements the set
-          [^     ] matches at beginning of line
-          [$     ] matches at end of line
-          [\|    ] (infix) alternative between two expressions
-          [\(..\)] grouping and naming of the enclosed expression
-          [\1    ] the text matched by the first [\(...\)] expression
                    ([\2] for the second expression, etc)
-          [\b    ] matches word boundaries
-          [\     ] quotes special characters. *)
val regexp_case_fold: string -> regexp
        (* Same as [regexp], but the compiled expression will match text
           in a case-insensitive way: uppercase and lowercase letters will
           be considered equivalent. *)

(*** String matching and searching *)

(*
external string_match: regexp -> string -> int -> bool = "str_string_match"
        (* [string_match r s start] tests whether the characters in [s]
           starting at position [start] match the regular expression [r].
           The first character of a string has position [0], as usual. *)
external search_forward: regexp -> string -> int -> int = "str_search_forward"
        (* [search_forward r s start] searchs the string [s] for a substring
           matching the regular expression [r]. The search starts at position
           [start] and proceeds towards the end of the string.
           Return the position of the first character of the matched
           substring, or raise [Not_found] if no substring matches. *)
external search_backward: regexp -> string -> int -> int = "str_search_backward"
        (* Same as [search_forward], but the search proceeds towards the
           beginning of the string. *)
*)

val matched_string: string -> string
        (* [matched_string s] returns the substring of [s] that was matched
           by the latest [string_match], [search_forward] or [search_backward].
           The user must make sure that the parameter [s] is the same string
           that was passed to the matching or searching function. *)
val match_beginning: unit -> int
val match_end: unit -> int
        (* [match_beginning()] returns the position of the first character
           of the substring that was matched by [string_match],
           [search_forward] or [search_backward]. [match_end()] returns
           the position of the character following the last character of
           the matched substring. *)
val matched_group: int -> string -> string
        (* [matched_group n s] returns the substring of [s] that was matched
           by the [n]th group [\(...\)] of the regular expression during
           the latest [string_match], [search_forward] or [search_backward].
           The user must make sure that the parameter [s] is the same string
           that was passed to the matching or searching function. *)
val group_beginning: int -> int
val group_end: int -> int
        (* [group_beginning n] returns the position of the first character
           of the substring that was matched by the [n]th group of
           the regular expression. [group_end n] returns
           the position of the character following the last character of
           the matched substring. *)

(*** Replacement *)

val global_replace: regexp -> string -> string -> string
        (* [global_replace regexp repl s] returns a string identical to [s],
           except that all substrings of [s] that match [regexp] have been
           replaced by [repl]. The replacement text [repl] can contain
           [\1], [\2], etc; these sequences will be replaced by the text
           matched by the corresponding group in the regular expression.
           [\0] stands for the text matched by the whole regular expression. *)
val replace_first: regexp -> string -> string -> string
        (* Same as [global_replace], except that only the first substring
           matching the regular expression is replaced. *)
val global_substitute: regexp -> (string -> string) -> string -> string
        (* [global_substitute regexp subst s] returns a string identical
           to [s], except that all substrings of [s] that match [regexp]
           have been replaced by the result of function [subst]. The
           function [subst] is called once for each matching substring,
           and receives [s] (the whole text) as argument. *)
val substitute_first: regexp -> (string -> string) -> string -> string
        (* Same as [global_substitute], except that only the first substring
           matching the regular expression is replaced. *)

(*** Splitting *)

val split: regexp -> string -> string list
        (* [split r s] splits [s] into substrings, taking as delimiters
           the substrings that match [r], and returns the list of substrings.
           For instance, [split (regexp "[ \t]+") s] splits [s] into
           blank-separated words. *)
val bounded_split: regexp -> string -> int -> string list
        (* Same as [split], but splits into at most [n] substrings,
           where [n] is the extra integer parameter. *)

(*** Extracting substrings *)

val string_before: string -> int -> string
        (* [string_before s n] returns the substring of all characters of [s]
           that precede position [n] (excluding the character at 
           position [n]). *)
val string_after: string -> int -> string
        (* [string_after s n] returns the substring of all characters of [s]
           that follow position [n] (including the character at 
           position [n]). *)
val first_chars: string -> int -> string
        (* [first_chars s n] returns the first [n] characters of [s].
           This is the same function as [string_before]. *)
val last_chars: string -> int -> string
        (* [last_chars s n] returns the last [n] characters of [s]. *)
end
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

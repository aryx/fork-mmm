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

val create : ?random:bool -> int -> ('a,'b) t
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

type statistics

module type S =
  sig
    type key
    type 'a t
    val create : int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
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

    val stats: 'a t -> statistics
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

val hash_param : int -> int -> 'a -> int
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

type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6

type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4

module Printf : sig
(* Module [Printf]: formatting printing functions *)
val sprintf : ('a, unit, string) format -> 'a
        (* Same as [printf], but return the result of formatting in a
           string. *)

val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
        (* Same as [fprintf], but instead of printing on an output channel,
           append the formatted arguments to the given extensible buffer
           (see module [Buffer]). *)

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

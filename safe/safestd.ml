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

external format_int: string -> int -> string = "caml_format_int"
external format_float: string -> float -> string = "caml_format_float"

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
type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6
type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4
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

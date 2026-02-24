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

(* type 'a option = 'a Stdlib.option = None | Some of 'a *)

(* Exceptions *)

external raise : exn -> 'a = "%raise"

let failwith = Stdlib.failwith
let invalid_arg = Stdlib.invalid_arg

exception Exit
exception Assert_failure of (string * int * int)

(* Comparisons *)

external (=) : 'a -> 'a -> bool = "%equal"
external (<>) : 'a -> 'a -> bool = "%notequal"
external (<) : 'a -> 'a -> bool = "%lessthan"
external (>) : 'a -> 'a -> bool = "%greaterthan"
external (<=) : 'a -> 'a -> bool = "%lessequal"
external (>=) : 'a -> 'a -> bool = "%greaterequal"
external compare: 'a -> 'a -> int = "caml_compare"

let min = Stdlib.min
let max = Stdlib.max

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

let abs = Stdlib.abs

external (land) : int -> int -> int = "%andint"
external (lor) : int -> int -> int = "%orint"
external (lxor) : int -> int -> int = "%xorint"

let lnot = Stdlib.lnot

external (lsl) : int -> int -> int = "%lslint"
external (lsr) : int -> int -> int = "%lsrint"
external (asr) : int -> int -> int = "%asrint"

let min_int = Stdlib.min_int
let max_int = Stdlib.max_int

(* Floating-point operations *)

external (~-.) : float -> float = "%negfloat"
external (+.) : float -> float -> float = "%addfloat"
external (-.) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external (/.) : float -> float -> float = "%divfloat"
let ( ** ) = Stdlib.( ** )
let exp = Stdlib.exp
let acos = Stdlib.acos
let asin = Stdlib.asin
let atan = Stdlib.atan
let atan2 = Stdlib.atan2
let cos = Stdlib.cos
let cosh = Stdlib.cosh
let log = Stdlib.log
let log10 = Stdlib.log10
let sin = Stdlib.sin
let sinh = Stdlib.sinh
let sqrt = Stdlib.sqrt
let tan = Stdlib.tan
let tanh = Stdlib.tanh
let ceil = Stdlib.ceil
let floor = Stdlib.floor
external abs_float : float -> float = "%absfloat"
let mod_float = Stdlib.mod_float
let frexp = Stdlib.frexp
let ldexp = Stdlib.ldexp
let modf = Stdlib.modf
external float : int -> float = "%floatofint"
external float_of_int : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"

(* String operations -- more in module String *)

external string_length : string -> int = "%string_length"

let (^) = Stdlib.(^)

(* Character operations -- more in module Char *)

external int_of_char : char -> int = "%identity"
let char_of_int = Stdlib.char_of_int

(* Unit operations *)

external ignore : 'a -> unit = "%ignore"

(* Pair operations *)

external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"

(* String conversion functions *)

external format_int: string -> int -> string = "caml_format_int"
external format_float: string -> float -> string = "caml_format_float"

let string_of_bool = Stdlib.string_of_bool
let bool_of_string = Stdlib.bool_of_string

let string_of_int = Stdlib.string_of_int

let int_of_string = Stdlib.int_of_string

let string_of_float = Stdlib.string_of_float

let float_of_string = Stdlib.float_of_string

(* List operations -- more in module List *)

let (@) = Stdlib.(@)

(* I/O operations *)
(* for applets, we only keep stdout and stderr output, and stdin input *)

let print_char = Stdlib.print_char
let print_string = Stdlib.print_string
let print_int = Stdlib.print_int
let print_float = Stdlib.print_float
let print_endline = Stdlib.print_endline
let print_newline = Stdlib.print_newline

(* Output functions on standard error *)

let prerr_char = Stdlib.prerr_char
let prerr_string = Stdlib.prerr_string
let prerr_int = Stdlib.prerr_int
let prerr_float = Stdlib.prerr_float
let prerr_endline = Stdlib.prerr_endline
let prerr_newline = Stdlib.prerr_newline

(* Input functions on standard input *)

let read_line = Stdlib.read_line
let read_int = Stdlib.read_int
let read_float = Stdlib.read_float

(* References *)

type 'a ref = 'a Stdlib.ref = { mutable contents: 'a }
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
module Stack = Stack
module Stream = Stream
module Buffer = Buffer
module String = String
(* Sys: nothing useful *)
(* Weak: advanced... *)

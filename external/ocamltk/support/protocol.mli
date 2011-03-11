open Widget

(* Lower level interface *)
exception TkError of string
      (* Raised by the communication functions *)

val debug : bool ref 
      (* When set to true, displays approximation of intermediate Tcl code *)

type tkArgs =
    TkToken of string
  | TkTokenList of tkArgs list		(* to be expanded *)
  | TkQuote of tkArgs			(* mapped to Tcl list *)


(* Misc *)
external splitlist : string -> string list
      	= "camltk_splitlist"

val add_destroy_hook : (widget -> unit) -> unit


(* Opening, closing, and mainloop *)
val default_display : unit -> string

val opentk : string list -> widget
    (* The basic initialization function. [opentk argv] takes command line
       options [argv] for Tk applications, like ["-display"; "localhost:0"]. 
       Application name can be specified by ["-name"; name] *)

val keywords : (string * Arg.spec * string) list
    (* Command line parsing specification for Arg.parse, which contains
       the standard Tcl/Tk command line options such as "-display" and "-name".
       These Tk command line options are stored for opentk_with_parsed_args *)
val opentk_with_parsed_args : unit -> widget
    (* [opentk_with_parsed_args ()] invokes [opentk] with the command line 
       options given to the executable program. Before calling 
       [opentk_with_parsed_args], the command line arguments must be parsed
       by [Arg.parse] using [keywords]. *)

val openTk : unit -> widget
    (* [openTk ()] is equivalent to [opentk ["-name"; "Camltk"]] *)
val openTkClass: string -> widget
    (* [openTkClass class] is equivalent to [opentk ["-name"; class]] *)
val openTkDisplayClass: string -> string -> widget
    (* [openTkDisplayClass disp class] is equivalent to 
       [opentk ["-display"; disp; "-name"; class]] *)

val closeTk : unit -> unit
val finalizeTk : unit -> unit 
    (* Finalize tcl/tk before exiting. This function will be automatically 
       called when you call [Pervasives.exit ()] *)

val mainLoop : unit -> unit


(* Direct evaluation of tcl code *)
val tkEval : tkArgs array -> string

val tkCommand : tkArgs array -> unit

(* Returning a value from a Tcl callback *)
val tkreturn: string -> unit


(* Callbacks: this is private *)

type cbid

type callback_buffer = string list
      (* Buffer for reading callback arguments *)

val callback_naming_table : (cbid, callback_buffer -> unit) Hashtbl.t
val callback_memo_table : (widget, cbid) Hashtbl.t
      (* Exported for debug purposes only. Don't use them unless you
         know what you are doing *)
val new_function_id : unit -> cbid
val string_of_cbid : cbid -> string
val register_callback : widget -> (callback_buffer -> unit) -> string
      (* Callback support *)
val clear_callback : cbid -> unit
      (* Remove a given callback from the table *)
val remove_callbacks: widget -> unit
      (* Clean up callbacks associated to widget. Must be used only when
      	 the Destroy event is bind by the user and masks the default
	 Destroy event binding *)

val cTKtoCAMLwidget : string -> widget
val cCAMLtoTKwidget : string list -> widget -> tkArgs

val register : string -> (callback_buffer -> unit) -> unit

(*-*)
val prerr_cbid : cbid -> unit

open Tk
open Feed
open Www
open Hyper

type logger = {
  logger_destroy : bool -> unit;
  logger_progress : int -> unit;
  logger_msg : string -> unit;
  logger_end : string -> unit
}

(* Document Id is a reference to a document in the browser
   For some documents, e.g. results of POST queries, the URL is not a
   sufficient description. Stamp is 0 for unique documents.
*)
type document_id = {
  document_url : Url.t;
  document_stamp : int
  }

module DocumentIDSet =
  Set.Make(struct type t = document_id let compare = compare end)

type handle = {
  document_id : document_id;
  document_referer : string option;
  mutable document_status : int;
  mutable document_headers : string list;
  document_feed : Feed.t;
  document_fragment : string option;
  mutable document_logger : logger
}

type document_continuation = {
  document_process : handle -> unit;
  document_finish : bool -> unit
}

type document_data =
   MemoryData of Ebuffer.t
 | FileData of string * bool (* flag is true if file is temporary *)

type document = {
  document_address : Url.t;
  mutable document_data : document_data;
  document_info : string list
  }

let stamp_counter = ref 0
let no_stamp = 0

let new_stamp () =
  incr stamp_counter; !stamp_counter

let document_id wwwr =
  match wwwr.www_link.h_method with
    POST _  ->
      	 { document_url = wwwr.www_url; document_stamp = new_stamp()}
  | _ -> { document_url = wwwr.www_url; document_stamp = no_stamp}


(* Close a connexion. Should be called only by a fileinput callback
      or by somebody attempting to abort the connexion 
   We remove the fd of the select before closing it since we don't want
   a spurious read to happen. This way we are somewhat independant of the
   Tk implementation 
 *)

let dclose remactive dh =
  dh.document_feed.feed_unschedule();
  dh.document_feed.feed_close();
  if remactive then Www.rem_active_cnx dh.document_id.document_url


let tty_logger = 
  { logger_destroy = (fun _ -> ());
    logger_progress = (fun _ -> ());
    logger_msg = Log.f;
    logger_end = Log.f
   }
  
let add_log dh initmsg abort =
  let t = 
    Toplevel.create Widget.default_toplevel [Class "MMMLog"] in
  Wm.withdraw t;
  Wm.title_set t 
      (I18n.sprintf "Document log %s" 
         (Url.string_of dh.document_id.document_url)); 
  let l = Label.create t 
    [Text initmsg; Justify Justify_Left; WrapLength (Pixels 600)]
  and fprog, set_progress = Frx_fillbox.new_horizontal t 200 10 
  and b = Button.create t 
      [Text (I18n.sprintf "Abort"); 
       Command (fun () -> dclose true dh; abort(); destroy t)] in
    pack [l;fprog;b][];
  let putmsg txt = 
    Label.configure l [Text txt] in
  let finished msg =
    putmsg msg;
    Button.configure b 
      	[Text (I18n.sprintf "Ok"); 
         Command (fun () -> if Winfo.exists t then destroy t)] in
  let iconified = ref true in
  let logger = {
    logger_destroy =
      (fun delayed ->
	if Winfo.exists t then
	  if !iconified then (* that was fast *)
	    destroy t
	  else if not delayed then destroy t
	  else
              Timer.set 5000 (fun () -> if Winfo.exists t then destroy t));
    logger_progress = 
      (fun n -> if Winfo.exists t then set_progress n);
    logger_msg =
      (fun msg -> if Winfo.exists t then putmsg msg);
    logger_end = 
      (fun msg -> if Winfo.exists t then finished msg) } in

   dh.document_logger <- logger;
   (* The logger appears only after a given delay *)
     Timer.set 3000
      (fun () -> if Winfo.exists t then (Wm.deiconify t; iconified := false))

let put_log dh = dh.document_logger.logger_msg
and destroy_log dh = dh.document_logger.logger_destroy
and progress_log dh = dh.document_logger.logger_progress
let end_log dh msg =
    dh.document_logger.logger_end msg;
    destroy_log dh true


type display_info = {
    di_abort : unit -> unit;
    di_destroy : unit -> unit;
    di_fragment : string option -> unit;
    di_redisplay: unit -> unit;
    di_title : unit -> string;	      (* some title for bookmarks *)
    di_source : unit -> unit;
    di_load_images : unit -> unit
}


open Tk
open Document

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

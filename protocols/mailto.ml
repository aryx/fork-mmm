(*s: ./protocols/mailto.ml *)
(* mailto: *)
open Sys
open Tk
open Unix
open Frx_text
open Www
open Hyper
open Url

(*s: constant Mailto.mailer *)
let mailer = ref ""
(*e: constant Mailto.mailer *)

(*s: enum Mailto.msg *)
type msg = {
  dest : string;
  subject : string;
  body : string 
  }
(*e: enum Mailto.msg *)

(*s: function Mailto.error *)
let error body =
  try
    let oc = open_out_bin (Filename.concat (getenv "HOME") "dead.letter") in
     output_string oc body;
     close_out oc;
     Error.default#f (I18n.sprintf "Can't send mail (saved in $HOME/dead.letter)")
  with
     _ -> 
      Error.default#f (I18n.sprintf "Can't send mail, can't save dead.letter")
(*e: function Mailto.error *)

  (* if the mail contains a dot line, we're f*cked *)
(*s: function Mailto.sendmail *)
let sendmail msg =
 let cmd = try Sys.getenv "MMM_MAIL" with Not_found -> "mail" in
 try
  let (fd_in,fd_out) = pipe() in
  match Low.fork () with
    0 -> close fd_out; dup2 fd_in stdin;
     Munix.execvp cmd [| cmd; "-s"; msg.subject; msg.dest |]
  | n -> close fd_in;
     Munix.write_string fd_out msg.body;
     close fd_out;
     begin match waitpid [] n with
       _, WEXITED 0 -> Error.default#ok (I18n.sprintf "Mail sent")
     | _, _ -> error msg.body
     end
 with
   Unix_error(_,_,_) -> error msg.body
(*e: function Mailto.sendmail *)

(*s: function Mailto.internal *)
let internal address referer =
  let top = 
    Toplevel.create Widget.default_toplevel [Class "MMMMail"] in
  let dest = Textvariable.create_temporary top
  and subject = Textvariable.create_temporary top in
     Textvariable.set dest address;
     Textvariable.set subject referer;
  let fto,eto = Frx_entry.new_labelm_entry top "To:" dest
  and fsub, esub = Frx_entry.new_labelm_entry top "Subject:" subject
  and fbody, tbody = new_scrollable_text top [] false in

    pack [fto; fsub][Fill Fill_X];

  let fbut = Frame.create top [] in
  let bok = Button.create fbut 
      [Text (I18n.sprintf "Send"); 
       Command 
      	 (fun () -> 
       let msg = 
      	     {dest = Textvariable.get dest;
          (* Japanese e-mails must be in JIS code, but they must be *)
          (* already in JIS --- JPF *)
          subject = Textvariable.get subject; 
          body = Text.get tbody (TextIndex(LineChar(0,0), [])) textEnd
      	     } in
       sendmail msg;
      	   destroy top)]
  and bdismiss = Button.create fbut 
      [Text (I18n.sprintf "Dismiss"); Command (fun () -> destroy top)] in
    pack [bok] [Side Side_Left];
    pack [bdismiss] [Side Side_Right];
    pack [fbut][Side Side_Bottom; Fill Fill_X];

    (* last for resizing *)
    pack [fbody][Expand true; Fill Fill_Both]
(*e: function Mailto.internal *)


   
(*s: function Mailto.get *)
let get mailaddr referer =
 let subject = match referer with
     None -> "no subject"
   | Some s -> "About url "^s  in
  match !mailer with
     "" ->
       internal mailaddr subject
   | s -> 
       ignore
        (Munix.system_eval s 
          ["_", "-s"; "SUBJECT", subject; "TO", mailaddr] true)
(*e: function Mailto.get *)

(*s: function Mailto.f *)
let f wr =
  match wr.www_url.path with
    None -> wr.www_error#f (I18n.sprintf "No address given for mailto:")
  | Some rawaddress ->
     let address = Urlenc.decode rawaddress in
       match wr.www_link.h_method with
     GET -> get address wr.www_link.h_context
       | POST d ->
       if wr.www_error#choose 
           (I18n.sprintf "About to send mail with POST data to\n%s"
                 address)
       then
         let subject = match wr.www_link.h_context with
         None -> "no subject"
           | Some s -> "POST data for "^s in
          sendmail 
        { dest = address; subject = subject; body = d}
       else ()
       | _ ->
       wr.www_error#f (I18n.sprintf "Unsupported method for mailto:")
(*e: function Mailto.f *)
(*e: ./protocols/mailto.ml *)

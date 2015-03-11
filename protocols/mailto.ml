(*s: ./protocols/mailto.ml *)
(* mailto: *)
open I18n
open Sys
open Unix
open Www
open Hyper
open Url

(*s: constant Mailto.mailer *)
let mailer = ref ""
(*e: constant Mailto.mailer *)

(*s: type Mailto.msg *)
type msg = {
  dest    : string;
  subject : string;
  body    : string 
}
(*e: type Mailto.msg *)

(*s: function Mailto.error *)
let error body =
  try
    let oc = open_out_bin (Filename.concat (getenv "HOME") "dead.letter") in
    output_string oc body;
    close_out oc;
    Error.f (s_ "Can't send mail (saved in $HOME/dead.letter)")
  with _ -> 
    Error.f (s_ "Can't send mail, can't save dead.letter")
(*e: function Mailto.error *)

(*s: function Mailto.sendmail *)
(* if the mail contains a dot line, we're f*cked *)
let sendmail msg =
  let cmd = try Sys.getenv "MMM_MAIL" with Not_found -> "mail" in
  try
    let (fd_in,fd_out) = pipe() in
    match Low.fork () with
    | 0 -> 
        close fd_out; 
        dup2 fd_in stdin;
        Munix.execvp cmd [| cmd; "-s"; msg.subject; msg.dest |]
    | n -> 
       close fd_in;
       Munix.write_string fd_out msg.body;
       close fd_out;
       (match waitpid [] n with
       | _, WEXITED 0 -> Error.ok (s_ "Mail sent")
       | _, _ -> error msg.body
       )
 with Unix_error(_,_,_) -> error msg.body
(*e: function Mailto.sendmail *)

(*s: global Mailto.internal_backend *)
let internal_backend = 
  ref (fun _ _ -> failwith "no Mailto.internal defined")
(*e: global Mailto.internal_backend *)

(*s: function Mailto.internal *)
let internal address referer =
  !internal_backend address referer
(*e: function Mailto.internal *)
   
(*s: function Mailto.get *)
let get mailaddr referer =
  let subject = 
    match referer with
    | None -> "no subject"
    | Some s -> "About url "^s  
  in
  match !mailer with
   | "" -> internal mailaddr subject
   | s -> 
       Munix.system_eval s ["_", "-s"; "SUBJECT", subject; "TO", mailaddr] true
        |> ignore
(*e: function Mailto.get *)

(*s: function Mailto.f *)
let f wr =
  match wr.www_url.path with
  | Some raw_address ->
      let address = Urlenc.decode raw_address in
      (match wr.www_link.h_method with
      | GET -> get address wr.www_link.h_context
      | POST d ->
          if wr.www_error#choose 
            (s_ "About to send mail with POST data to\n%s" address)
          then
            let subject = 
              match wr.www_link.h_context with
              | None -> "no subject"
              | Some s -> "POST data for "^s 
            in
            sendmail  { dest = address; subject = subject; body = d}
          else ()
     | _ ->
        wr.www_error#f (s_ "Unsupported method for mailto:")
     )
  | None -> wr.www_error#f (s_ "No address given for mailto:")
(*e: function Mailto.f *)
(*e: ./protocols/mailto.ml *)

(*s: http/auth.ml *)
(* HTTP Basic Authentication *)

open I18n
open Unix
open Http_headers
open Www

(*s: type [[Auth.authSpace]] *)
(* Authorizations are remembered on the base of the directory url and realm
 * They are kept during the whole MMM session, with expiration
 *)
type authSpace = {
   auth_proxy: bool;
   auth_host : string;
   auth_port : int;
   auth_dir : string;
   auth_realm : string
  }
(*e: type [[Auth.authSpace]] *)

(*s: type [[Auth.authEntry]] *)
type authEntry = {
   auth_cookie : string;
   mutable auth_lastused : float
   }
(*e: type [[Auth.authEntry]] *)

(*s: constant [[Auth.authorizations]] *)
let authorizations = Hashtbl.create 37
(*e: constant [[Auth.authorizations]] *)


(*s: function [[Auth.get]] *)
let get space = 
  let entry = Hashtbl.find authorizations space in
    entry.auth_lastused <- Unix.time();
    entry.auth_cookie
(*e: function [[Auth.get]] *)

(*s: constant [[Auth.lifetime]] *)
(* Lifetime, in minutes. Default is one hour *)
let lifetime = ref 60
(*e: constant [[Auth.lifetime]] *)


(*s: function [[Auth.lookup]] *)
let rec lookup space = 
  (* Printf.eprintf "%s\n" space.auth_dir; flush Pervasives.stderr; *)
  try
    Hashtbl.find authorizations space
  with
    Not_found ->
     if space.auth_dir = "/" || space.auth_dir = "." 
     then raise Not_found 
     else
      let newdir = Filename.dirname space.auth_dir in
       lookup {auth_proxy = space.auth_proxy;
               auth_host = space.auth_host;
            auth_port = space.auth_port;
        auth_dir = newdir;
        auth_realm = space.auth_realm}
(*e: function [[Auth.lookup]] *)

let open_passwd_ref = ref (fun _ -> failwith "no Auth.open_passswd defined")
(*s: function [[Auth.ask_cookie]] *)
let ask_cookie forwhere =
  try
    let u,p = !open_passwd_ref forwhere in
    Base64.encode (u^":"^p)
  with
  | Failure "cancelled" -> 
      failwith "cancelled"
  | _ -> 
      Error.f (s_ "Error in base 64 encoding");
      failwith "cancelled"
(*e: function [[Auth.ask_cookie]] *)

(*s: function [[Auth.replace]] *)
let replace kind cookie l =
  let rec repl acc = function
    [] -> (kind,cookie)::acc
  | (k,_)::l when k = kind -> repl (acc) l
  | p::l -> repl (p::acc) l in
  repl [] l
(*e: function [[Auth.replace]] *)
  

(*s: function [[Auth.add]] *)
let add space cookie =
  Log.debug "adding cookie";
  Hashtbl.add authorizations 
      space 
      {auth_cookie = cookie; auth_lastused = Unix.time()}
(*e: function [[Auth.add]] *)

(*s: function [[Auth.check]] *)
(* Kind is either: realm or proxy *)
let check wwwr challenge authspace =
  let kind = if authspace.auth_proxy then "proxy" else "realm" in
  match challenge.challenge_scheme with
    AuthExtend _ -> (* we don't know how to do this *) 
       None
  | AuthBasic -> (* params are gleefully ignored *)
     try (* if the passwd request is cancelled *)
      let cookie, isnew =
        if List.mem_assoc kind wwwr.www_auth then begin
           (* we already tried, so the authorization is bad ! *)
           Hashtbl.remove authorizations authspace; (* in case *)
           ask_cookie (s_ "Authorization for %s \"%s\" on \
                                             %s:%d/%s" 
                            kind challenge.challenge_realm 
                            authspace.auth_host authspace.auth_port 
                            authspace.auth_dir),
           true
           end
       else (* ah, it is our first try,  get the authorization *)
         if authspace.auth_proxy then 
            ask_cookie (s_ "Authorization for %s \"%s\" on \
                                             %s:%d/%s" 
                            kind challenge.challenge_realm
                            authspace.auth_host authspace.auth_port 
                            authspace.auth_dir),
            true
         else
           try 
             let entry = lookup authspace in
              entry.auth_lastused <- Unix.time();
              entry.auth_cookie, false
           with Not_found ->
            ask_cookie (s_ "Authorization for %s \"%s\" on \
                                             %s:%d/%s" 
                            kind challenge.challenge_realm
                            authspace.auth_host authspace.auth_port 
                            authspace.auth_dir),
            true
      in
      wwwr.www_auth <- replace kind cookie wwwr.www_auth;
      Some (cookie, isnew, authspace)
     with
      Failure "cancelled" -> None
(*e: function [[Auth.check]] *)

let edit_backend = ref (fun _ -> failwith "no Auth.edit defined") 

(* Authorisation control *)
(*s: function [[Auth.edit]] *)
(* needs to be refined *)
let edit () =
  !edit_backend ()
(*e: function [[Auth.edit]] *)

(*s: constant [[Auth.auth_file]] *)
(* Saving authorizations to file *)
let auth_file = ref ""
(*e: constant [[Auth.auth_file]] *)

(*s: function [[Auth.save]] *)
let save () =
 if !auth_file <> "" then
  let auth_file = Msys.tilde_subst !auth_file in
  try
    let o = openfile auth_file [O_WRONLY; O_CREAT] 0o600 in
    let oc = out_channel_of_descr o in
      output_value oc authorizations;
      flush oc;
      close o
  with
  | Unix_error(e,_,_) ->
      Error.f (s_ "Error in authorisation save\n%s" (Unix.error_message e))
  | Sys_error s ->
      Error.f (s_ "Error in authorisation save\n%s" s)
 else 
   Error.f (s_ "No authorisation file defined")
(*e: function [[Auth.save]] *)

(*s: function [[Auth.load]] *)
let load () =
  if !auth_file <> "" then
    let auth_file = Msys.tilde_subst !auth_file in
    try
      let ic = open_in auth_file in
      let table = input_value ic
      and time = Unix.time() in
       Hashtbl.iter
          (fun spacerealm entry ->
           entry.auth_lastused <- time;
           Hashtbl.add authorizations spacerealm entry)
          table;
    close_in ic
    with Sys_error s ->
      Error.f (s_ "Error in authorisation load\n%s" s)
 else 
   Error.f (s_ "No authorisation file defined")
(*e: function [[Auth.load]] *)

(*s: function [[Auth.init]] *)
let init () =
  let check () =
    let remove = ref []
    and lifetime = float (60 * !lifetime)
    and time = Unix.time () in
    Hashtbl.iter 
      (fun space entry ->
       let expiration_time = entry.auth_lastused +. lifetime in
    if time > expiration_time then remove := space :: !remove)
      authorizations;
    List.iter (Hashtbl.remove authorizations) !remove
  in
  let rec tim () =
    Timer_.set (!lifetime * 30000) (fun () -> check(); tim ())
  in
  tim ()
(*e: function [[Auth.init]] *)
(*e: http/auth.ml *)

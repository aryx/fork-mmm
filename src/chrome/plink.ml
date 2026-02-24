(*s: gui/plink.ml *)
open I18n
open Tk
open Hyper

(*s: function [[Plink.dial]] *)
let dial hlink err =
  let t = Toplevel.create Widget.default_toplevel [Class "Dialog"] in
  Focus.set t;
  Wm.title_set t (s_ "Malformed link error");

  let vuri = Textvariable.create_temporary t 
  and vcontext = Textvariable.create_temporary t in

  Textvariable.set vuri hlink.h_uri;
  (match hlink.h_context with
    Some s -> Textvariable.set vcontext s
  | None -> ());

  let msg = match err with
      LinkResolve s -> s
   |  UrlLexing (s,_) -> s in

  let tit = Label.create t [Text (s_ "Malformed link error")]
  and fc,_ec = Frx_entry.new_labelm_entry t "Context" vcontext
  and fu,eu = Frx_entry.new_labelm_entry t "Relative" vuri
  and lmsg = Label.create t [Text msg]
  in
  let cancelled = ref false in
  let fb = Frame.create t [] in
    let bok = Button.create fb
            [Text "Ok"; Command (fun _ -> Grab.release t; destroy t)]
    and bcancel = Button.create fb
            [Text "Cancel"; Command (fun _ -> cancelled := true;
                                          Grab.release t; destroy t)]
    in

    pack [bok] [Side Side_Left; Expand true];
    pack [bcancel] [Side Side_Right; Expand true];
    pack [tit;fc;fu;lmsg;fb] [Fill Fill_X];
    Tkwait.visibility t;
    Focus.set eu;
    Grab.set t;
    Tkwait.window t;
    (* because the window gets destroyed, the variables too. *)
    if !cancelled then None
    else Some
         {h_uri = Textvariable.get vuri;
      h_context = (match Textvariable.get vcontext with
                 "" -> None
                | s -> Some s);
          h_method = hlink.h_method;
      h_params = hlink.h_params}
(*e: function [[Plink.dial]] *)

(*s: function [[Plink.make]] *)
(* Utility for catching link resolving errors *)
let rec make hlink =
  try
    Www.make hlink
  with
    Invalid_link msg ->
      match dial hlink msg with
    None -> raise (Invalid_link msg)
      | Some hlink -> make hlink
(*e: function [[Plink.make]] *)
(*e: gui/plink.ml *)

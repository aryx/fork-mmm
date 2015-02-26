open Sys
open Unix

open Tk
open Frx_text

open Www
open Hyper
open Url

open Mailto


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

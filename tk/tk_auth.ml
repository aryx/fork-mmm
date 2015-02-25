open Tk
open Unix
open Http_headers
open Url
open Www
open Auth


let edit () =
  let top =
    Toplevel.create Widget.default_toplevel 
         [Class "MMMAuthorizations"] in
    Wm.title_set top (I18n.sprintf "Authorizations");
    let f,lb = Frx_listbox.new_scrollable_listbox top [TextWidth 40] in
      Hashtbl.iter (fun space cookie ->
       Listbox.insert lb End
      [Printf.sprintf "(%s) http://%s:%d/%s" 
         space.auth_realm space.auth_host space.auth_port space.auth_dir])
       authorizations;
    let buts = Frame.create top [] in
    let clearb = Button.create_named buts "clear"
       [Text (I18n.sprintf "Clear"); 
       Command (fun _ -> Hashtbl.clear authorizations; destroy top)]
    and dismissb = Button.create_named buts "dismiss"
       [Text (I18n.sprintf "Dismiss"); Command (fun _ -> destroy top)] in
      pack [clearb] [Side Side_Left; Expand true];
      pack [dismissb] [Side Side_Right; Expand true];
      pack [buts][Side Side_Bottom; Fill Fill_X];
      pack [f][Side Side_Top; Fill Fill_Both; Expand true]

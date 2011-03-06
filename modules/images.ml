open Safe418mmm

(* This module demonstrates
    - how to add an user menu
    - how to call the HTML lexer
*)

module Provide = struct
  let capabilities = Capabilities.get()
  end

module Net = Retrieval(Provide)
module Mmm = Get(Provide)

open Tk
open Net
open Html
open Document
open Feed
open Hyper
open Viewers

let images lexbuf =
  let uris = ref [] in
  try
    let lexer = ParseHTML.sgml_lexer Dtd.dtd32 in
    while true do
      try 
        let _,_,tokens,loc = lexer lexbuf in
	  List.iter (function
	     OpenTag {tag_name = "img"; attributes = attrs} ->
	       begin try
		uris := List.assoc "src" attrs :: !uris
	       with Not_found -> ()
	       end
	   | EOF -> raise End_of_file
	   | _ -> ())
          tokens
      with
        Html_Lexing _ -> ()
      | Invalid_Html _ -> ()
    done;
    !uris
  with
    End_of_file -> List.rev !uris

(* Pops up a dialog box with the list of image URLs *)
let show_images ctx l =
  let w = Applets.get_toplevel_widget [] 
  and base = Url.string_of (ctx#base.document_url)
  in
  Wm.withdraw w;
  Frx_req.open_list "Display Images" l
    (fun uri -> 
      let link = 
	{h_uri = uri; h_context = Some base; h_method = GET; h_params = []} in
        ctx#goto link)
    (fun _ -> destroy w)

(* When the menu item is activated, this function is called :
   we're interested mostly in the URL of the currently displayed document,
   but the ctx will be used later so we can trigger new navigation functions
   on the URLs of the in-lined images.
   What we do is request a copy of this document, on which we run an HTML
   lexer.
 *)
let f ctx =
  let cont = {
    document_process = (fun dh ->
      let lexbuf = Lexing.from_function
	               (fun buf n -> dh.document_feed.feed_read buf 0 n) in
      let l = images lexbuf in
        dclose true dh;
        show_images ctx l);
    document_finish = (fun _ -> ())
    } in
  let link = {h_uri = Url.string_of ctx#base.document_url;
	      h_context = None;
	      h_method = GET;
	      h_params = []} in

  Net.retrieve link cont

let _ = Mmm.add_user_menu "In-lined images" f


let _ = Applets.register "main"
    (fun f ctx ->
      pack [Label.create f [Text "Menu User/images installed"]][])

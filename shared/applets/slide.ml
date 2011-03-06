open Safe418

open Tk
open Tkanim
open Document
open Viewers
open Hyper
open Capabilities

module Provide = struct
  let capabilities = Capabilities.get()
  end

module Net = Retrieval(Provide)
open Net

let load_image base w delay loop l =
  let rec animate = function
  | [] -> if loop then animate l
  | url :: rest ->
     (* Retrieve the image *)
     get_image {h_uri = url; h_context = Some base;
		h_method = GET; h_params = []}
       (* Continuation of image loading *)
       (fun url i ->
	 match i with
	 | Still o -> 
	     if Winfo.exists w then begin
	       Label.configure w [o];
	       Timer.set delay (fun () -> animate rest)
       	     end
	 | _ -> ())
  in
  animate l


(* The applet can be invoked with
<EMBED SRC="../applets/slide.cmo">
<PARAM NAME="function" VALUE=f>
<PARAM VALUE="url of some image">
...
</EMBED>
*)
let f w ctx =
  let l = Label.create w [Text "Slide Show"] in
    pack [l][];
    let delay = 
     try int_of_string (List.assoc "delay" ctx#params) with _ -> 3000
    and urls = ref [] in
       List.iter (function (name,value) when name = "url" ->
			      urls := value :: !urls
			 | _ -> ())
		 ctx#params;
      let base = Url.string_of ctx#base.document_url in
      load_image base l delay false (List.rev !urls)

let loop w ctx =
  let l = Label.create w [Text "Looping Slide Show"] in
    pack [l][];
    let delay = 
     try int_of_string (List.assoc "delay" ctx#params) with _ -> 3000
    and urls = ref [] in
       List.iter (function (name,value) when name = "url" ->
			      urls :=  value :: !urls
			 | _ -> ())
		 ctx#params;
      let base = Url.string_of ctx#base.document_url in
      load_image base l delay true (List.rev !urls)

(* Register the applet function. *)
let _ = Applets.register "f" f
let _ = Applets.register "loop" loop
let _ = Applets.register "main" loop

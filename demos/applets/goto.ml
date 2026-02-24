open Safe418

open Tk
open Viewers
open Hyper

let f frame ctx =
  try
    let url = List.assoc "url" ctx#params
    and title = List.assoc "title" ctx#params in
    let link = {h_uri = url;
                h_context = None;
		h_method = GET;
	        h_params = []} in
    let t = Button.create frame [Text title;
			     Command (fun _ -> ctx#goto link)]
     in
     pack [t][]
  with
    Not_found -> failwith "Navigation not available"

let _ = Applets.register "f" f
let _ = Applets.register "main" f

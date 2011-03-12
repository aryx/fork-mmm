open Safe418mmm
open Tk
open Hyper
open Viewers
open Document

module Provide = struct
  let capabilities = Capabilities.get()
  end
module Mmm = Get(Provide)


(* Defines embedded viewer for audio types as re-running the document *)
let fake_embed media_pars w ctx dh =
  Document.dclose true dh;
  try 
    let hlink = {h_uri = Url.string_of dh.document_id.document_url;
		 h_context = None;
		 h_method = GET;
		 h_params = []} in
    pack [Label.create w [Text "Redispatched externally"]][];
    ctx#goto hlink
  with 
    Not_found (* goto *) -> 
      pack [Label.create w [Text "No navigation given to us"]][]
  | e ->
      pack [Label.create w [Text (Printexc.to_string e)]][]
let _ =
  Mmm.add_embedded_viewer ("audio", "*") fake_embed

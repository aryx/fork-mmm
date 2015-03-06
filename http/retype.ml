(*s: ./http/retype.ml *)
open Document
open Http_headers

(*s: function Retype.f *)
(* Attempt to find a decent Content-Type *)
let f dh =
  let url = Url.string_of dh.document_id.document_url in
  try
    let ctype = Http_headers.contenttype dh.document_headers in 
    let mtyp,_pars = Lexheaders.media_type ctype in
    if mtyp = ("application","octet-stream") 
    then dh.document_headers <- merge_headers dh.document_headers (hints url)
  with Not_found ->
    let hints = Http_headers.hints url in
    dh.document_headers <- merge_headers dh.document_headers hints
(*e: function Retype.f *)


(*e: ./http/retype.ml *)

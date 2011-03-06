open Url

let protos = Hashtbl.create 11

let _ = Hashtbl.add protos FTP (Http.proxy_req, Cache.tobuffer)
let _ = Hashtbl.add protos HTTP (Http.req, Cache.tobuffer)
let _ = Hashtbl.add protos GOPHER (Http.proxy_req, Cache.tobuffer)
let _ = Hashtbl.add protos NEWS (Http.proxy_req, Cache.tobuffer)
let _ = Hashtbl.add protos NNTP (Http.proxy_req, Cache.tobuffer)
let _ = Hashtbl.add protos WAIS (Http.proxy_req, Cache.tobuffer)
let _ = Hashtbl.add protos FILE (File.request, Cache.dummy)

let get = Hashtbl.find protos

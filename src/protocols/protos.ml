(*s: protocols/protos.ml *)
open Common

(*s: constant [[Protos.protos]] *)
let protos : (Url.protocol, 
             (Www.request -> Document.continuation -> Www.aborter) * 
             (Document.handle -> Document.data * Cache.cache_fill)) Hashtbl.t =
  Hashtbl_.create ()
(*e: constant [[Protos.protos]] *)

(*s: toplevel [[Protos._1]] *)
let _ = Hashtbl.add protos Url.FTP (Http.proxy_req, Cache.tobuffer)
(*e: toplevel [[Protos._1]] *)
(*s: toplevel [[Protos._2]] *)
let _ = Hashtbl.add protos Url.HTTP (Http.req, Cache.tobuffer)
(*e: toplevel [[Protos._2]] *)
(*s: toplevel [[Protos._3]] *)
let _ = Hashtbl.add protos Url.GOPHER (Http.proxy_req, Cache.tobuffer)
(*e: toplevel [[Protos._3]] *)
(*s: toplevel [[Protos._4]] *)
let _ = Hashtbl.add protos Url.NEWS (Http.proxy_req, Cache.tobuffer)
(*e: toplevel [[Protos._4]] *)
(*s: toplevel [[Protos._5]] *)
let _ = Hashtbl.add protos Url.NNTP (Http.proxy_req, Cache.tobuffer)
(*e: toplevel [[Protos._5]] *)
(*s: toplevel [[Protos._6]] *)
let _ = Hashtbl.add protos Url.WAIS (Http.proxy_req, Cache.tobuffer)
(*e: toplevel [[Protos._6]] *)
(*s: toplevel [[Protos._7]] *)
let _ = Hashtbl.add protos Url.FILE (File.request, Cache.dummy)
(*e: toplevel [[Protos._7]] *)

(*s: constant [[Protos.get]] *)
let get = Hashtbl.find protos
(*e: constant [[Protos.get]] *)
(*e: protocols/protos.ml *)

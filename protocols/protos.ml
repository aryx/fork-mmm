(*s: protocols/protos.ml *)
open Url

(*s: constant [[Protos.protos]] *)
let protos = Hashtbl.create 11
(*e: constant [[Protos.protos]] *)

(*s: toplevel [[Protos._1]] *)
let _ = Hashtbl.add protos FTP (Http.proxy_req, Cache.tobuffer)
(*e: toplevel [[Protos._1]] *)
(*s: toplevel [[Protos._2]] *)
let _ = Hashtbl.add protos HTTP (Http.req, Cache.tobuffer)
(*e: toplevel [[Protos._2]] *)
(*s: toplevel [[Protos._3]] *)
let _ = Hashtbl.add protos GOPHER (Http.proxy_req, Cache.tobuffer)
(*e: toplevel [[Protos._3]] *)
(*s: toplevel [[Protos._4]] *)
let _ = Hashtbl.add protos NEWS (Http.proxy_req, Cache.tobuffer)
(*e: toplevel [[Protos._4]] *)
(*s: toplevel [[Protos._5]] *)
let _ = Hashtbl.add protos NNTP (Http.proxy_req, Cache.tobuffer)
(*e: toplevel [[Protos._5]] *)
(*s: toplevel [[Protos._6]] *)
let _ = Hashtbl.add protos WAIS (Http.proxy_req, Cache.tobuffer)
(*e: toplevel [[Protos._6]] *)
(*s: toplevel [[Protos._7]] *)
let _ = Hashtbl.add protos FILE (File.request, Cache.dummy)
(*e: toplevel [[Protos._7]] *)

(*s: constant [[Protos.get]] *)
let get = Hashtbl.find protos
(*e: constant [[Protos.get]] *)
(*e: protocols/protos.ml *)

(*s: http/http.mli *)

(*s: exception [[Http.End_of_headers]] *)
exception End_of_headers
(*e: exception [[Http.End_of_headers]] *)

(*s: signature [[Http.read_headers]] *)
(* [read_headers fd]
 *  reads HTTP headers from a fd
 *    raises End_of_file
 *    raises Invalid_HTTP_header
 *)
val read_headers:
  (bytes -> int -> int -> int) -> string list -> string list
(*e: signature [[Http.read_headers]] *)

(*s: exception [[Http.HTTP_error]] *)
exception HTTP_error of string
(*e: exception [[Http.HTTP_error]] *)

(*s: signature [[Http.req]] *)
val req: < Cap.network; ..> ->
  Www.request -> Document.continuation -> Www.aborter
(*e: signature [[Http.req]] *)

(*s: signature [[Http.proxy_xxx]] *)
val proxy: string ref
val proxy_port: int ref
(*e: signature [[Http.proxy_xxx]] *)

(*s: signature [[Http.proxy_req]] *)
val proxy_req: < Cap.network; ..> ->
  Www.request -> Document.continuation -> Www.aborter
(*e: signature [[Http.proxy_req]] *)

val always_proxy: bool ref
val send_referer: bool ref
val user_agent: string ref
val timeout: int ref

val verbose: bool ref
(*e: http/http.mli *)

(*s: ./viewers/decoders.ml *)
open Unix
open Document
open Feed
open Http_headers

(*s: constant Decoders.decoders *)
(* Insert a decoding if necessary.
 * We don't do it in http, since we don't want do decompress when we
 * save for example.
 *)
let decoders = Hashtbl.create 37
(*e: constant Decoders.decoders *)

(*s: function Decoders.gzip *)
(* Note: we must use the feed interface to read from the old dh,
 * and not read directly from the feed_internal file descriptor, because
 * the feed might implement side effects (such as caching).
 * Since we are reading and writing to the same process, we might get
 * deadlocked if we don't watch writes.
 *)
let gzip dh =
  let (gread, mwrite) = pipe()
  and (mread, gwrite) = pipe()
  in
  Unix.set_close_on_exec mread;
  Unix.set_close_on_exec mwrite;
  
  match Low.fork() with
    0 ->  
      dup2 gread stdin; dup2 gwrite stdout;
      Munix.execvp "gunzip" [| "gunzip"; "-c" |]
      (* dh (* fake *) *)
  | _n ->  
      close gread; close gwrite;
     (* it is safe to close feed because the son has a copy *)
      let newdh =
       { dh with 
         dh_headers = rem_contentencoding dh.dh_headers;
         document_feed = Feed.of_fd mread;
       }
      in
      let buffer : bytes = Bytes.create 4096 in
      let rec copy () =
      try
        let n = dh.document_feed.feed_read buffer 0 4096 in
            if n = 0 then (dclose true dh; close mwrite)
            else begin
          dh.document_feed.feed_unschedule();
          Fileevent.add_fileoutput mwrite
        (fun () -> 
          ignore (write mwrite buffer 0 n);
          Fileevent.remove_fileoutput mwrite;
          dh.document_feed.feed_schedule copy)
        end
      with
        Unix_error(_e,_,_) -> dclose true dh; close mwrite
      in
      dh.document_feed.feed_schedule copy;
      newdh
(*e: function Decoders.gzip *)
  

(*s: toplevel Decoders._1 *)
let _ =  
  [ "COMPRESS"   , gzip;
    "X-COMPRESS" , gzip;
    "GZIP"       , gzip;
    "X-GZIP"     , gzip
  ] |> List.iter (fun (s,t) -> Hashtbl.add decoders s t)
(*e: toplevel Decoders._1 *)

(*s: constant Decoders.add *)
let add = Hashtbl.add decoders
(*e: constant Decoders.add *)

(*s: function Decoders.insert *)
let insert dh =
(* CERN proxy sets Content-Encoding when return code = 500 ! *)
  if dh.document_status >= 400 then dh else
  try
    Hashtbl.find decoders (String.uppercase_ascii (contentencoding dh.dh_headers)) dh
  with
    Not_found -> dh
  | Unix_error(_,_,_) -> dh
(*e: function Decoders.insert *)

(*e: ./viewers/decoders.ml *)

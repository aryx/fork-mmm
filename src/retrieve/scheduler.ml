(*s: retrieve/scheduler.ml *)
(* Scheduled downloading *)

(*s: constant [[Scheduler.debug]] *)
let debug = ref false
(*e: constant [[Scheduler.debug]] *)

(*s: type [[Scheduler.progress_func]] *)
type progress_func = int option -> int -> unit
(*e: type [[Scheduler.progress_func]] *)

(* Handling of data downloaded by this scheduler *)
module type Data = sig
  type t

  val load : Document.handle -> Document.id list -> Fpath.t -> t
  (* [load dh referers file] *)

  val cache_access : Url.t -> Document.id -> t
  (* [cache_access url referer] *)

  val error :
    Url.t -> (Document.id * ((Url.t -> t -> unit) * progress_func)) list -> unit
  (* [error url conts] *)

  val error_msg : Www.request * string -> unit
  (* Retrieval produces Invalid_url *)
end

module type S = sig
  type shared_data

  val add_request :
    < Cap.network > ->
    Www.request ->
    Document.id ->
    (Url.t -> shared_data -> unit) ->
    progress_func ->
    unit
  (* [add_request wwwr ref_did cont progress_func] *)

  val stop : Document.id -> unit
  (* [stop ref_did] *)

  (* Delayed queues for this scheduler *)
  type delayed

  val new_delayed : unit -> delayed

  val add_delayed :
    delayed ->
    Www.request ->
    Document.id ->
    (Url.t -> shared_data -> unit) ->
    progress_func ->
    unit

  val flush_delayed : delayed -> unit
  val flush_one : < Cap.network > -> delayed -> Url.t -> unit
  val is_empty : delayed -> bool
  val maxactive : int ref
  val maxsamehost : int ref
end

module Make (J : Data) = struct
  type shared_data = J.t

  let maxactive = ref 10
  let maxsamehost = ref 2

  (* A job is: a list of referers, with the continuations *)
  type job = {
    mutable stop : unit -> unit;
    mutable conts :
      (Document.id * ((Url.t -> shared_data -> unit) * progress_func)) list;
    mutable bytes_loaded : int;
    mutable contentlength : int option;
  }

  (* The list of active requests : this is used to share the requests
     for all jobs on the same Url. *)
  let active = ref 0
  and actives = (Hashtbl.create 11 : (Url.t, job) Hashtbl.t)

  (* We need a two-level queue system, so that
     1- we respect the image loading order for each document
     2- we can use maxactive connexions
     3- there is a max of maxsamehost connexions on the same host
  *)

  let samehost = (Hashtbl.create 11 : (string, int ref) Hashtbl.t)
  (* count of cnx on each host (IP number is best choice), but for
     performance reason (DNS lookups), we take FQDN *)

  let addhost (url : Url.t) =
    let s =
      match url.host with
      | Some s -> s
      | None -> ""
    in
    try
      let count = Hashtbl.find samehost s in
      if !count < !maxsamehost then (
        incr count;
        true)
      else false
    with
    | Not_found ->
        Hashtbl.add samehost s (ref 1);
        true (* assumes maxsamehost >= 1 *)

  let remhost (url : Url.t) =
    let s =
      match url.host with
      | Some s -> s
      | None -> ""
    in
    try
      let r = Hashtbl.find samehost s in
      decr r;
      if !r <= 0 then Hashtbl.remove samehost s
    with
    | Not_found -> () (* that's an error actually *)

  type queue =
    (Www.request * Document.id * (Url.t -> shared_data -> unit) * progress_func)
    Queue.t
  (* queue for one batch of docs *)

  let queues = (ref [] : queue list ref)
  (* pending queues for documents *)

  (* How we pick the next request *)

  exception Busy

  let skip_cache (wr : Www.request) =
    try Http_headers.get_header "pragma" wr.www_headers = "no-cache" with
    | Not_found -> false

  let pick () =
    let pick_in_batch q =
      try
        let (wr : Www.request), _, _, _ = Queue.peek q in
        let url = wr.www_url in
        if addhost url then Some (Queue.take q) else None
      with
      | Queue.Empty ->
          (* this batch is empty *)
          raise Queue.Empty
    in
    let rec walk_batches remaining = function
      | [] ->
          (* we've reached the end : reset the remaining scheduled jobs *)
          queues := List.rev remaining;
          raise Busy
      | x :: l -> (
          try
            match pick_in_batch x with
            | Some r -> r
            | None ->
                (* nothing pickable yet, look further *)
                walk_batches (x :: remaining) l
          with
          | Queue.Empty ->
              (* this queue is empty ! *)
              walk_batches remaining l)
    in
    walk_batches [] !queues

  (* Whenever we add something in the queue, we must call this *)
  (* Whenever a job finished, we must call this *)
  let rec next_request caps =
    if !active < !maxactive then
      try
        let j = pick () in
        process_request caps j;
        next_request caps (* check if more can be done *)
      with
      | Busy -> ()

  (* when adding a request individually (meant to be treated ASAP), we
     use a new singleton queue *)
  and add_request (caps : < Cap.network ; .. >) (wr : Www.request)
      (did : Document.id) cont (prog : progress_func) =
    let q = Queue.create () in
    Queue.add (wr, did, cont, prog) q;
    queues := q :: !queues;
    next_request caps

  (* error during data downloading *)
  and error (caps : < Cap.network; .. >) (url : Url.t) (job : job) =
    job.stop ();
    J.error url job.conts;
    if !debug then
      Logs.warn (fun m -> m "Retrieval of %s failed\n" (Url.string_of url));
    next_request caps

  (* process_request always follows pick, thus hostcount has always been
   * incremented for the URL of this request *)
  and process_request (caps : < Cap.network ; .. >) (wr, did, cont, prog) =
    try
      (* if we are in the cache of shared objects, apply continuation *)
      if skip_cache wr then raise Not_found
      else begin
        let data = J.cache_access wr.www_url did in
        remhost wr.www_url;
        (* we're done *)
        cont wr.www_url data
      end
    with
    | Not_found -> (
        (* find out if we are in the active jobs *)
        let url = wr.www_url in
        try
          let oldjob = Hashtbl.find actives url in
          (* then add a new continuation *)
          oldjob.conts <- (did, (cont, prog)) :: oldjob.conts;
          remhost wr.www_url
          (* we're done *)
        with
        | Not_found -> begin
            (* start a new job *)
            if !debug then
              Logs.debug (fun m -> m "Starting job for %s" (Url.string_of url));
            let job =
              {
                stop =
                  (fun () ->
                    Hashtbl.remove actives url;
                    decr active;
                    remhost url);
                conts = [ (did, (cont, prog)) ];
                contentlength = None;
                bytes_loaded = 0;
              }
            in
            (* Add to set of active *)
            incr active;
            Hashtbl.add actives url job;

            (* We are now going to run the retrieval process *)

            (* Continuations for the retrieval *)
            (* TODO: remove those nested function, move out and pass down
             * necessary closed arguments
             *)
            let handle_data (dh : Document.handle) =
              (* add more things to do in stop *)
              let oldstop = job.stop in
              job.stop <-
                (fun () ->
                  Document.dclose true dh;
                  oldstop ());
              try
                (* open the temporary file in which doc is to be saved *)
                let file = Msys.mktemp "data" in
                let oc = open_out file and buffer = Bytes.create 2048 in

                (* JPF HACK -- for Image retrieval progress meter *)
                begin
                  try
                    job.contentlength <-
                      Some (Http_headers.contentlength dh.dh_headers)
                  with
                  | Not_found -> ()
                end;

                (* actually start sucking data *)
                dh.document_feed.feed_schedule (fun () ->
                    try
                      let n = dh.document_feed.feed_read buffer 0 2048 in

                      (* JPF HACK -- for Image retrieval progress meter *)
                      job.bytes_loaded <- job.bytes_loaded + n;
                      List.iter
                        (fun (_, (_, prog)) ->
                          prog job.contentlength job.bytes_loaded)
                        job.conts;

                      if n <> 0 then output oc buffer 0 n
                      else begin
                        (* end of document *)
                        Document.dclose true dh;
                        (* see comment below *)
                        close_out oc;
                        (* proceed to load and run continuations *)
                        let referers = List.map fst job.conts in
                        begin
                          try
                            let data = J.load dh referers (Fpath.v file) in
                            List.iter
                              (fun (_referer, (cont, _)) ->
                                try
                                  Printexc.print
                                    (cont dh.document_id.document_url)
                                    data
                                with
                                | _ -> flush Stdlib.stderr)
                              job.conts
                          with
                          (* load failed *)
                          | e ->
                              Logs.warn (fun m -> m "Load error %s"
                                            (Printexc.to_string e));
                              J.error url job.conts
                        end;
                        (* we must remove from active only after
                           loading because otherwise, if loading is interactive,
                           there could be a moment during which the document
                           is not marked as loaded, but not active either.
                           This would cause multiple retrievals.
                           But then dh has to be closed otherwise the
                           callback will we called indefinitely *)
                        oldstop ();
                        if !debug then
                          Logs.debug (fun m -> m
                            "Finished job for %s" (Url.string_of url));
                        (* proceed with more requests *)
                        next_request caps
                      end
                    with
                    (* errors in retrieval *)
                    | Unix.Unix_error (code, s, s') ->
                        Logs.err (fun m -> m "Unix error (%s) in scheduler %s %s"
                                              (Unix.error_message code) s s');
                        close_out oc;
                        error caps url job
                    | Sys_error s ->
                        Logs.err (fun m -> m "IO error (%s) in scheduler" s);
                        close_out oc;
                        error caps url job
                    | e ->
                        Logs.err (fun m -> m
                          "Bug in scheduler %s" (Printexc.to_string e));
                        close_out oc;
                        error caps url job)
              with
              (* error creating tmp file *)
              | Sys_error s ->
                  Logs.err (fun m -> m "Can't create temporary file (%s)" s);
                  error caps url job
              | e ->
                  Logs.err (fun m -> m "Bug in scheduler %s" (Printexc.to_string e));
                  error caps url job


            (* Data has moved. The best way to do this properly is to
               reschedule the job conts as new requests *)
            and retry_data (hlink : Hyper.link) =
              try
                job.stop ();
                let newr = Www.make hlink in
                newr.www_error <- wr.www_error;
                newr.www_logging <- wr.www_logging;
                job.conts |> List.iter (fun (did, (cont, prog)) ->
                    add_request caps newr did cont prog)
              with
              (* can't proceed with retry *)
              | _ -> error caps url job
            in


            (* Okay, go for the retrieval now *)
            try
              match
                Retrieve.f caps wr retry_data
                  {
                    document_process = handle_data;
                    document_finish = (fun f -> if f then error caps url job);
                  }
              with
              | Retrieve.Started _ -> ()
              | Retrieve.InUse ->
                  (* somebody else has started a request bypassing the
                     scheduler, dammit. Our only hope is that he's going
                     to set the cache properly, so we can reschedule
                     ourself and try later *)
                  job.stop ();
                  List.iter
                    (fun (did, (cont, prog)) ->
                      add_request caps wr did cont prog)
                    job.conts
            with
            | Www.Invalid_request (w, msg) ->
                (* retrieve failed *)
                J.error_msg (w, msg);
                error caps url job
          end)

  (*
   * And now, various utilities
   *)

  (* remove pending requests whose referer is did *)
  let stop (did : Document.id) =
    (* For all queues, for all request in the queue, if the request matches
       the predicate, it is removed from the queue. *)
    queues :=
      !queues |> List.map (fun q ->
             let newq = Queue.create () in
             q |> Queue.iter (function
                  | _wr, didr, _cont, _progress when did = didr -> ()
                  | r -> Queue.add r newq);
             newq);

    (* If the request is active, remove the particular continuation, and if it
       was the only continuation, kill the job
    *)
    let rem = ref [] in
    (* jobs to kill *)
    actives |> Hashtbl.iter
      (fun _url job ->
        try
          job.conts <- Mlist.except_assoc did job.conts;
          if job.conts = [] then rem := job :: !rem
        with
        | Not_found -> ()
    );
    (* each stop closes the cnx properly and remove the job from actives *)
    !rem |> List.iter (fun job -> job.stop ());
    if !rem <> [] then
      let caps = Cap.network_caps_UNSAFE () in
      next_request caps

  (*
   * Delayed queues
   *)
  type delayed = queue

  let new_delayed = Queue.create

  let is_empty (q : queue) =
    try
      Queue.peek q |> ignore;
      false
    with
    | Queue.Empty -> true

  (* add a new request in the queue *)
  (* Actually, if the document is already in the cache, then process
     the continuation *)
  let add_delayed (q : delayed) (wr : Www.request) did cont progress =
    try
      if skip_cache wr then raise Not_found
      else cont wr.www_url (J.cache_access wr.www_url did)
    with
    | Not_found -> Queue.add (wr, did, cont, progress) q

  (* Put the queue in the list of queues *)
  let flush_delayed (q : delayed) : unit =
    (* Queue.iter (function (_,_,_,prog) -> prog None 0) q;(* create the gauge *) *)
    queues := !queues @ [ q ];
    let caps = Cap.network_caps_UNSAFE () in
    next_request caps

  (* Flush a particular request from a queue : we do it in place
     because we don't know if the queue has been put in the list yet
  *)
  let flush_one (caps : < Cap.network >) (l : delayed) (url : Url.t) : unit  =
    let flushedqueue = Queue.create () and restqueue = Queue.create () in
    (* split in two *)
    l |> Queue.iter (function
        | (wr : Www.request), did, cont, prog when wr.www_url = url ->
            prog None 0;
            (* create the gauge *)
            Queue.add (wr, did, cont, prog) flushedqueue
        | r -> Queue.add r restqueue
    );
    (* the flushed goes at the beginning *)
    queues := flushedqueue :: !queues;
    (* copy back the remaining in l (MUST BE THE SAME l) *)
    Queue.clear l;
    restqueue |> Queue.iter (fun r -> Queue.add r l);
    (* try to process the flushed items *)
    next_request caps
end
(*e: retrieve/scheduler.ml *)

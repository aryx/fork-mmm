(*s: ./commons/feed.ml *)
(*s: type Feed.internal *)
(* An abstract notion of connection *)

type internal = Unix.file_descr
(*e: type Feed.internal *)

(*s: type Feed.t *)
type t = {
  feed_read : string -> int -> int -> int;
  feed_schedule : (unit -> unit) -> unit;
  feed_unschedule : unit -> unit;
  feed_close : unit -> unit;
  feed_internal : internal  
  }
(*e: type Feed.t *)

(*s: function Feed.of_fd *)
(* We should distinguish internal/external connections *)
let of_fd fd =
  let is_open = ref true
  and action = ref None
  and condition = Condition.create()
  and first_read = ref false
  in
  (* ASSUMES: this is the first read on the fileevent *)
  let safe_read buf offs len =
    first_read := false;
    if !is_open then Low.read fd buf offs len else 0
  in
  (* In other cases : this is non blocking but not fully threaded. *)
  let special_read buf ofs len =
     (* remove the normal handler *)
     Fileevent.remove_fileinput fd; 
     (* add a handler to trigger the condition *)
     Fileevent.add_fileinput fd (fun () ->
     Fileevent.remove_fileinput fd; (* remove myself *)
     Condition.set condition);
     (* wait for the condition to happen *)
     Condition.wait condition;
     (* Meanwhile, someone may have unscheduled/closed the 
    feed (e.g. abort). We call safe_read, but if the feed has been
    closed, read will fail.
    To know if we have to put back on schedule, check the *current*
    state of action
      *)
     let n = try safe_read buf ofs len with _ -> 0 in
     (* reschedule; it is essential that Low.add_fileinput does not
        call the event loop, otherwise we loose sequentiality of reads *)
     (match !action with
        Some f ->
      Fileevent.add_fileinput fd (fun () -> first_read := true; f())
      | None -> ());
     (* and return *)
     n
  in {
   feed_read =
    (fun buf ofs len -> 
       if !first_read then safe_read buf ofs len
       else special_read buf ofs len);
   feed_schedule = 
    (function f ->
       if not !is_open then
     Log.f "ERROR: feed is closed, can't schedule"
       else match !action with
     Some f -> (* we are already scheduled ! *)
       Log.f "Warning: feed already scheduled"
       | None -> begin
        action := Some f;
        Low.add_fileinput fd (fun () -> first_read := true; f())
      end);
   feed_unschedule = 
    (function () -> 
       match !action with
     Some f -> Low.remove_fileinput fd; action := None
       | None -> (* this happens quite often (for all action codes which
            do not process the body of the document, the feed got
            unscheduled as the end of headers) *)
       ());

   (* feed_close must be called only if the feed it *not* scheduled *)
   feed_close =
    (function () -> 
       (* if we abort during a state when we are waiting on the condition,
          the feed is unscheduled but we never get out. So always change
      the state *)
       Condition.set condition;
       if !is_open then begin
     match !action with 
        Some f -> Log.f "ERROR: feed is scheduled, can't close"
      | None -> Unix.close fd;
                is_open := false;
            (* Condition.free condition RACE CONDITION HERE *)
       end);
   feed_internal = fd
     }
(*e: function Feed.of_fd *)
 
(*s: function Feed.internal *)
let internal {feed_internal = fd} = fd
(*e: function Feed.internal *)

(*e: ./commons/feed.ml *)

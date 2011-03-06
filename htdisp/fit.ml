open Printf
open Tk
open Protocol

let debug = ref false

(* initial width : a nested formatter starts with w=1 h=1
 * if the only contents is an embedded window, it's a bit short
 * thus we check for max width of embedded windows
 * We take an arbitrary width of 10 pixels per char.
 *)
let set_initial_width wid =
  let ewidth = ref 0 in
  List.iter (fun w -> ewidth := max (Winfo.reqwidth w) !ewidth) 
            (Text.window_names wid);
  if !ewidth > 10 then begin
    let w = !ewidth / 10 in
    if !debug then
      Log.f (sprintf "Setting initial width of %s to %d"
   	             (Widget.name wid) w);
    Text.configure wid [TextWidth w];
    Some w
  end
  else
    None

(* initial height: we have to grow at least to the number of
 * lines of text so that the entire text is visible. Moreover,
 * large lines will fold so adjust. Moreover, embedded window
 * provide height.
 *)
(* I still don't understand the difference between height and reqheight *)
let wheight w = max (Winfo.height w) (Winfo.reqheight w)

let set_initial_height wid =
  match Text.index wid (TextIndex(End,[])) with
    LineChar (l,_) ->
      let height = ref (l-1) in
      for i = 0 to l - 1 do
	match Text.index wid (TextIndex(LineChar(i,0), [LineEnd])) with
	  LineChar (_,c) -> height := !height + c / 100
	| _ -> ()
      done;
      begin
	let embedded = Text.window_names wid in
	if embedded = [] then () 
	else begin
	  let lines = Hashtbl.create (List.length embedded) in
	  let addh l h =
	    try
	      let r = Hashtbl.find lines l in
	      r := max h !r
	    with
	      Not_found -> Hashtbl.add lines l (ref h)
	  in
	  List.iter (fun w ->
	    match Text.index wid (TextIndex(Embedded w, [])) with
	      LineChar(l,_) -> addh l (wheight w)
	    | _ -> assert false)
	    embedded;
	  Hashtbl.iter (fun _ r -> height := !height + !r / 15) lines
      	end;
    	let curheight = int_of_string (cget wid CHeight) in
	if !height > curheight then begin
	  if !debug then 
	    Log.f (sprintf "Setting initial height of %s to %d (%d)"
   	                   (Widget.name wid) !height (l-1));
      	  Text.configure wid [TextHeight (!height)]
	end else if !debug then 
	    Log.f (sprintf "Initial height of %s is %d (%d)"
   	                   (Widget.name wid) !height (l-1))
      end
  | _ -> ()


(* Grow horizontally until we reached the maxium authorized width 
 * (or bound is reached)
 *)
let rec fixed_horiz wid maxw =
  let curw = Winfo.reqwidth wid in
  if  curw >= maxw then ()
  else begin
    let w = (succ (int_of_string (cget wid CWidth))) in
    if !debug then
      Log.f (sprintf "Growing %s to %d (w=%d) (max=%d)"
	             (Widget.name wid) w curw maxw);
    Text.configure wid [TextWidth w];
    fixed_horiz wid maxw
  end

let horiz wid stop continuation =
  (* all conditions for stopping *)
  let finished visible = visible >= 0.999 || stop()
  (* bail out *)
  and over () = 
   (* deconnect ourselves *)
   (*Text.configure wid [XScrollCommand (fun _ _ -> ())];*)
   continuation()
  and last_visible = ref 0.0 
  in
  (* if we want to restart after we were disconnected*)
  let rec check () = 
    let first, last = Text.xview_get wid in 
     scroll first last
  (* binding to XScrollCommand *)
  and scroll first last =
    let curwidth = int_of_string (cget wid CWidth)
    and visible = last -. first in
    (* Don't attempt anything if widget is not visible *)
    (* Especially, DO NOT DECIDE TO STOP *)
    if not (Winfo.viewable wid) then begin
      if !debug then
	Log.f (sprintf "%s HC %d %f %f notviewable"
	               (Widget.name wid) curwidth first last);
      (* Try again later *)
      bind wid [[], Expose] (BindSet ([], fun _ ->
	bind wid [[], Expose] BindRemove;
	check()))
    end
    else if finished visible then over()
    else if visible = !last_visible then 
      (* it didn't change since our last resize ! *)
      ()
    else begin
      (* how much do we need to grow
	 This code is disabled because it causes masking of table cells
	 (we don't have a reasonable estimation of a minimum horiz growth
	  that would avoid masking). We now grow by 1, despite the 
	  slowness.
      let delta = 
	if last = 0.0 then 1
        else begin
	  last_visible := visible;
	  let visible = max 0.2 visible in
	  let missing = 1. -. visible in
	  (* at least one char, but not too much *)
	  let computed = truncate (float curwidth *. missing /. visible) in
	  if computed = 0 then 1 else min 5 computed
        end
      in
      let newsize = curwidth + delta in
	 *)
      last_visible := visible;
      let newsize = curwidth + (if visible < 0.1 then 5 else 1) in
      if !debug then
      	Log.f (sprintf "%s H %d %f %f newsize: %d"
	               (Widget.name wid) curwidth first last newsize);
      Text.configure wid [TextWidth newsize];
    end
	
  in
  scroll, check


(* somehow we need to do it differently : resize is delayed *)
let vert wid =
  let finished visible = visible >= 0.999
  and last_visible = ref (-1.0)   (* last value of visible *)
  and stuck = ref false     (* last resize didn't have an effect *)
  and pending_check = ref false
  and delayed = ref false   (* we have a binding on Expose *)
  and pending_resize = ref false (* a resize is pending *)
  and newsize = ref 0
  in
  let rec check () = 
    if Winfo.exists wid then begin (* we must check since we use a delay *)
    let first, last = Text.yview_get wid in 
      pending_check := false;
      scroll first last
    end
  and scroll first last =
    let curheight = int_of_string (cget wid CHeight)
    and visible = last -. first in
    (* Don't attempt anything if widget is not visible *)
    if not (Winfo.viewable wid) then begin
      if !debug then
	Log.f (sprintf "%s VC %d %f %f notviewable"
	               (Widget.name wid) curheight first last);
      (* Try again later *)
      if not !delayed then begin
	delayed := true;
      	bind wid [[], Expose] (BindSet ([], fun _ ->
	  bind wid [[], Expose] BindRemove;
	  delayed := false;
	  check()))
      end
    end
    else if finished visible then ()
    else if !stuck then
      if !pending_check then ()
      else begin
      (* last check had same last value than before *)
      pending_check := true;
      if !debug then Log.f (sprintf "Stuck %s" (Widget.name wid));
      Timer.set 50 (fun () -> stuck := false; Frx_after.idle check)
      end
    else begin
      let delta = 
 	if visible = !last_visible then (stuck := true; 1) 
        else if last = 0.0 then (last_visible := 0.0; 1)
	else begin
	  last_visible := visible; stuck := false;
          (* never to more than double *)
	  let visible = max 0.5 visible in
	  let missing = 1. -. visible in
	  (* at least one char, but not too much *)
	  let computed = truncate (float curheight *. missing /. visible) in
	  if computed = 0 then 1 else min 5 computed
        end 
      in
      newsize := max (curheight + delta) !newsize;
      (* Since we may not be fully visible anyway, decouple the loop *)
      if !pending_resize then ()
      else begin
	if !debug then 
	  Log.f (sprintf "Scheduling resize of %s" (Widget.name wid));
	pending_resize := true;
        Timer.set 50 (fun () -> Frx_after.idle (resize first last))
      end
    end
  and resize first last () =
    pending_resize := false;
    let curheight = int_of_string (cget wid CHeight) in
    if !newsize > curheight then begin
      if !debug then
      	Log.f (sprintf "%s V %d %f %f newsize: %d"
		       (Widget.name wid) curheight first last !newsize);
      Text.configure wid [TextHeight !newsize]
    end
  in
  scroll, check

let bound_check wid width =
  let stop_now = ref false in
  bind wid [[], Configure]
    (BindExtend([Ev_Width], (fun ei ->
      if !debug then
	Log.f (sprintf "Configure %s width is %d (max %d) (req %d)"
	              (Widget.name wid) ei.ev_Width width
	              (Winfo.reqwidth wid));
      if ei.ev_Width >= width then begin
	stop_now := true;
 	bind wid [[], Configure] BindRemove
      end)));
  (fun () -> !stop_now)


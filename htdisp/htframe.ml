open Tk
open Html
open Htmlfmt

(* Frames *)

(* geometry specs *)
let geom_sep = Str.regexp "[ \t\n]+\|\([ \t\n]*,[ \t\n]*\)"
let parse_geom s = List.map Html.length_of_string (Str.split geom_sep s)

(* to deal with relative length n*, we have to combine relD and absD
 *   n* is n fragments of Total - Fixed
 *   = (Total - Fixed) * n/Sigma_n
 *   = Total * n/Sigma_n - Fixed * n/Sigma_n
 *   -> -relD (n/Sigma_n) -absD (-Fixed * n/Sigma_n)
 *)
let figure_geom l =
  (* compute the amount of fixed size *)
  let fixed = ref 0
  and totalrel = ref 0 in
  List.iter (function 
      LengthPixels n -> fixed := n + !fixed
    | LengthRel n -> totalrel := n + !totalrel
    | _ -> ())
    l;
  if !totalrel = 0 then (* simple case *)
    List.map (fun x -> [x]) l
  else
    List.map (function
      |	LengthRel n ->
	  let ratio = float n /. float !totalrel in
	  let opts = [LengthRatio (min ratio 1.)] in
	  if !fixed = 0 then opts
	  else
	    opts @ [LengthPixels (- (truncate (float !fixed *. ratio)))]
      |	x -> [x])
      l

(* We build this data structure when parsing FRAMESET *)
type frame = {
    frame_src : string;
    frame_name : string;
    frame_scrolling : string; (* yes | no | auto *)
    frame_opts : Tk.options list;
    frame_params : (string * string) list;
  } 

and frameset =
    int ref * int ref * celldesc array array

and cell_contents = 
  | Frame of frame
  | Frameset of frameset

and celldesc = {
    cell_width : Html.length list;
    cell_height : Html.length list;
    mutable cell_contents : cell_contents option
  } 

(* This is morally for the <noframes> section *)
let ignore_fo f = {
  new_paragraph = (fun () -> ());
  close_paragraph = (fun () -> ());
  print_newline = (fun b -> ());
  print_verbatim = (fun s -> ());
  format_string = (fun s -> ());
  hr = (fun l n b -> ());
  bullet = (fun n -> ());
  set_defaults = (fun s l -> ());
  push_attr = (fun l -> ());
  pop_attr = (fun l -> ());
  isindex = (fun s s' -> ());
  start_anchor = (fun () -> ());
  end_anchor = (fun h -> ());
  add_mark = (fun _ -> ());
  create_embedded = (fun a w h -> Frame.create f []);
  see_frag = (fun _ -> ());
  flush = (fun () -> ());
  } 

let ignore_close = fun _ -> ()


let add_frames load_frames kill_body top mach =
  (* we start from an initial cell of "full size" *)
  let initial_cell = { 
    cell_width = [LengthRatio 100.];
    cell_height = [LengthRatio 100.];
    cell_contents = None 
  } in
  (* framesets can be defined recursively, this is our context stack *)
  let framesets = ref ([] : frameset list) in
  (* each nested frame/frameset occupies a cell in its parent
   * the current cell to occupy is defined by ri/rj.
   *)
  let next_cell set ri rj =
    incr rj;
    if !rj >= Array.length set.(!ri) then begin
      rj := 0; incr ri
    end
  in

  (* Create the frames with the proper placing, launch the display *)
  let doit () =
    (* compute the real frames (the ones with embedded documents) *)
    let frames = ref ([] : (frame * Widget.widget) list) in
    let framesym = Mstring.egensym "framecell" in
    (* in some top window, place the given cell and proceed with its
     * contents recursively. [pos] defines the placing options in x/y
     *)
    let rec docell top cell pos = 
      let f = 
	Frame.create_named top 
	  (if cell == initial_cell then "frames"
	   else framesym()) [Class "HFrame"] in
      let place_opts = ref (In top :: pos) in
      (* the displacement caused by this cell in its parent *)
      let delta_x = ref 0 and delta_relx = ref 0.
      and delta_y = ref 0 and delta_rely = ref 0.
      in
      List.iter (function
      	| Nolength | LengthRel _ -> assert false
      	| LengthPixels n ->
	    place_opts := Width (Pixels n) :: !place_opts;
	    delta_x := n
      	| LengthRatio w -> 
	    place_opts := RelWidth w :: !place_opts;
	    delta_relx := w)
	cell.cell_width;
      List.iter (function
      	| Nolength | LengthRel _ -> assert false
      	| LengthPixels n ->
	    place_opts := Height (Pixels n) :: !place_opts;
	    delta_y := n
      	| LengthRatio w -> 
	    place_opts := RelHeight w :: !place_opts;
	    delta_rely := w)
	cell.cell_height;
      (* place the cell, unless it is the top cell *)
      if cell == initial_cell then begin
	pack [f] [Expand true; Fill Fill_Both];
	Pack.propagate_set f false
      end else place f !place_opts;
      (* proceed with its contents *)
      begin match cell.cell_contents with
      |	None -> () (* this is an error ! *)
      |	Some (Frame frame) ->
	  (* just store it so we can run the viewers later.
	     (we need to have all frames in order to give proper navigation
	     context for links with targets) *)
	  frames := (frame, f) :: !frames;
	  Frame.configure f frame.frame_opts
				    
      |	Some (Frameset (_,_,rows)) ->
	  (* this is again a frameset. Thus [f] is still only a container *)
	  (* positions of the embedded cells *)
	  let curabs_x = ref 0 and curabs_y = ref 0
	  and currel_x = ref 0. and currel_y = ref 0. in
	  (* placer options for each embedded cell *)
	  let curpos () = [X (Pixels !curabs_x); RelX !currel_x;
			   Y (Pixels !curabs_y); RelY !currel_y] in
	  (* Iterate on cells *)
	  Array.iter (fun row ->
	    (* for each row, we start horizontally at 0 *)
	    curabs_x := 0; currel_x := 0.;
	    (* the vertical size contributed by this row. It's constant
	       for all cells in the row, but we compute it n times...*)
	    let row_delta_y = ref 0
	    and row_delta_rely = ref 0. in
	    (* now iterate on each cell in this row (eg on columns) *)
	    Array.iter (fun cell -> 
	      (* place this cell and return its occupation *)
	      let delta_x, delta_relx, delta_y, delta_rely =
		docell f cell (curpos()) in
	      (* switch current horiz position, store vert occupation *)
	      curabs_x := delta_x + !curabs_x;
	      currel_x := delta_relx +. !currel_x;
	      row_delta_y := delta_y;
	      row_delta_rely := delta_rely
	      ) row;
	    (* we finished the row. Move vertically now *)
	    curabs_y := !curabs_y + !row_delta_y;
	    currel_y := !currel_y +. !row_delta_rely)
	    rows
      end;
      (* our caller expects us to return our size *)
      !delta_x, !delta_relx, !delta_y, !delta_rely
    in
    (* The initial cell is always at 0/0 *)
    ignore (docell top initial_cell [X (Pixels 0); Y (Pixels 0)]);
    (* And now proceed with frame loading *)
    load_frames !frames;
    (* some people put a body outside the noframes section, so we should
       ignore it completely if we saw frames. And we must kill the body
       if it was already created *)
    kill_body();
    mach#add_tag "body"
      (fun fo t -> mach#look_for EOF) ignore_close

  in
  mach#add_tag "frameset"
      (fun fo t ->
	let rows = get_attribute t "rows"
	and cols = get_attribute t "cols" in
	let newset =
	  ref 0, ref 0,
	  Array.of_list
	    (List.map (fun h ->  
	      Array.of_list (List.map  (fun w -> 
		{ cell_width = w;
		  cell_height = h;
		  cell_contents = None})
			       (figure_geom (parse_geom cols))))
	       (figure_geom (parse_geom rows)))
	in
	match !framesets with
	| [] -> 
	    (* if there two or more non-nested framesets, we will cause 
	       an error later *)
	    if initial_cell.cell_contents <> None then
	      raise (Invalid_Html "illegal <frameset>")
	    else begin
	      initial_cell.cell_contents <- Some (Frameset newset);
	      framesets := newset :: !framesets
	    end
	| (ri, rj, set)::l ->
	    if !ri >= Array.length set then
	      raise (Invalid_Html "no room for <frameset> in this <frameset>")
	    else begin
	      set.(!ri).(!rj).cell_contents <-  Some (Frameset newset);
	      next_cell set ri rj;
	      framesets := newset :: !framesets
	    end)
      (fun t -> 
	match !framesets with
	| [] -> 
	    raise (Invalid_Html "unmatched </frameset>")
	| [x] -> (* the last one *)
	    framesets := [];
	    doit()
	| x::l ->
	    framesets := l);

  mach#add_tag "frame"  
    (fun fo t ->
      match !framesets with
      | [] -> raise (Invalid_Html "<frame> outside <frameset>")
      |	(ri, rj, set) :: _ ->
	  if !ri >= Array.length set then
	    raise (Invalid_Html "no room for <frame> in this <frameset>")
	  else begin
	    try
	      let src = get_attribute t "src"
	      and name = try get_attribute t "name" with Not_found -> ""
	      and border = 
	      	try int_of_string (get_attribute t "frameborder")
	      	with Failure "int_of_string" -> 
		(* compatibility ? *)
		  if String.lowercase (get_attribute t "frameborder") = "no"
		  then 0 else 1
	      and scrolling = String.lowercase (get_attribute t "scrolling")
	      in
	      let borderopts = 
	      	if border = 0 then [BorderWidth (Pixels 0)]
	      	else [BorderWidth (Pixels border); Relief Ridge]
	      in
	      set.(!ri).(!rj).cell_contents <-
		 Some (Frame { frame_src = src;
			       frame_name = name;
			       frame_opts = borderopts;
			       frame_scrolling = scrolling;
			       frame_params = t.attributes });
	      next_cell set ri rj
	    with
	      Not_found -> 
	      	raise (Invalid_Html "missing src in <FRAME>")
	  end)
    ignore_close;

  (* note: <noframes> does not necessarily cover the whole body of the
   * document. It may only hide a toc which is displayed in another frame.
   * Basically, <noframes> doesn't imply there was a <frameset> in the
   * same document. Of course, we should interpret noframes ONLY if the
   * other frame supposed to contain the info IS displayed. But we don't
   * know that, do we ?
   *)
  mach#add_tag "noframes"
    (fun fo t -> 
      mach#push_formatter (ignore_fo top);
      mach#look_for (CloseTag "noframes"))

    (fun t -> mach#pop_formatter; ())


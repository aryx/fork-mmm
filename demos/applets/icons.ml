open Safe418

open Tk
open Tkanim
open Viewers
open Hyper
open Capabilities

module Provide = struct
  let capabilities = Capabilities.get()
  end

module Net = Retrieval(Provide)
open Net

(* 
 * Most of this canvas code is taken from Brent Welch's book.
 * However, his dragging code is bogus in the case the canvas
 * can be scrolled.
 *)

let scrollable_canvas parent opts =
  let f = Frame.create parent [] in
  let c = Canvas.create f opts
  and sx = Scrollbar.create_named f "xscroll" [Orient Horizontal]
  and sy = Scrollbar.create_named f "yscroll" [Orient Vertical]
  in
   Canvas.configure c 
      [XScrollCommand (Scrollbar.set sx); YScrollCommand (Scrollbar.set sy)];
   Scrollbar.configure sx [ScrollCommand (Canvas.xview c)];
   Scrollbar.configure sy [ScrollCommand (Canvas.yview c)];
   pack [sx][Side Side_Bottom; Fill Fill_X];
   pack [sy][Side Side_Right; Fill Fill_Y];
   pack [c][Side Side_Left; Fill Fill_Both; Expand true];
   f, c

let canvas_dragging w =
  let obj = ref None
  and x = ref 0
  and y = ref 0 in
  let mark ei =
    (* we must convert x/y into real canvas coords, since we might be
       viewing a subpart of the canvas *)
    let cx = truncate (Canvas.canvasx w (Pixels ei.ev_MouseX))
    and cy = truncate (Canvas.canvasy w (Pixels ei.ev_MouseY)) in
    match Canvas.find w [Closest (Pixels cx, Pixels cy)] with
      [] -> obj := None
    | i::_ -> obj := Some i; x:= cx; y := cy
  and drag ei =
    match !obj with
      None -> ()
    | Some i ->
      let cx = truncate (Canvas.canvasx w (Pixels ei.ev_MouseX))
      and cy = truncate (Canvas.canvasy w (Pixels ei.ev_MouseY)) in
       let dx = cx - !x
       and dy = cy - !y in
          Canvas.move w i (Pixels dx) (Pixels dy);
	  x := cx;
	  y := cy in
   Canvas.bind w (Tag "movable")
     [[], ButtonPressDetail 1] (BindSet ([Ev_MouseX; Ev_MouseY], mark));
   Canvas.bind w (Tag "movable")
     [[Button1], Motion] (BindSet ([Ev_MouseX; Ev_MouseY], drag))


let maxw = 500

let f w ctx =
 let fc, c = 
   scrollable_canvas w
    [Width (Pixels 500); Height (Pixels 300); 
     ScrollRegion (Pixels 0, Pixels 0, Pixels 800, Pixels 800)]
 and current_x = ref 0
 and current_y = ref 0 
 and current_lw = ref 0
 and current_lh = ref 0 in
    canvas_dragging c;
    pack[fc][Fill Fill_Both; Expand true];
    let add_icon url = function
    | Animated _ -> ()
    | Still o ->
        ignore 
          (Canvas.create_image c (Pixels !current_x) (Pixels !current_y) 
                  	       	 [o; Anchor NW; Tags [Tag "movable"]]);
      let w, h =
       match o with
       | ImageBitmap i -> 
	    Imagebitmap.width i, Imagebitmap.height i
       | ImagePhoto  i ->
            Imagephoto.width i, Imagephoto.height i
       | Bitmap n -> failwith "can't handle bitmap"
       | _ -> failwith "invalid image" in
       if !current_lw > maxw then begin
	 current_x := 0;
	 current_y := !current_y + !current_lh;
	 current_lh := h;
	 current_lw := w
	 end
       else begin
	 current_x := !current_x + w;
	 current_lw := !current_lw + w;
	 current_lh := max !current_lh h
	 end
   in
    List.iter (function (name, value) when name = "url" ->
                            get_image { h_uri = value;
				       	h_context = None;
					h_method = GET;
				        h_params = []}
	                              add_icon
                      |	 _ -> ())
              ctx#params

let _ = Applets.register "main" f


open Safe418

open Tk
open Viewers

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
    | [] -> obj := None
    | i::_ -> obj := Some i; x:= cx; y := cy

  and drag ei =
    match !obj with
    | None -> ()
    | Some i ->
        let cx = truncate (Canvas.canvasx w (Pixels ei.ev_MouseX))
        and cy = truncate (Canvas.canvasy w (Pixels ei.ev_MouseY)) in
        let dx = cx - !x
        and dy = cy - !y in
        Canvas.move w i (Pixels dx) (Pixels dy);
        x := cx;
        y := cy

  and delete ei =
    (* we must convert x/y into real canvas coords, since we might be
       viewing a subpart of the canvas *)
    let cx = truncate (Canvas.canvasx w (Pixels ei.ev_MouseX))
    and cy = truncate (Canvas.canvasy w (Pixels ei.ev_MouseY)) in
    match Canvas.find w [Closest (Pixels cx, Pixels cy)] with
    | [] -> ()
    | i :: _ -> Canvas.delete w [i]

  in
   Canvas.bind w (Tag "movable")
     [[], ButtonPressDetail 1] (BindSet ([Ev_MouseX; Ev_MouseY], mark));
   Canvas.bind w (Tag "movable")
     [[Button1], Motion] (BindSet ([Ev_MouseX; Ev_MouseY], drag));
   Canvas.bind w (Tag "movable")
     [[Control], ButtonPressDetail 1]
     (BindSet ([Ev_MouseX; Ev_MouseY], delete))

(* Compute the euclidean distance between to points *)
let distance (x, y) (x', y') =
  let dx = x - x'
  and dy = y - y' in
  truncate (sqrt (float (dx * dx + dy * dy)))

(* Compute the index of the min of a list *)
let min_index l =
  let rec m v cur n = function
    | [] -> n
    | x :: l -> if x < v then m x (succ cur) cur l else m v (succ cur) n l in
  match l with
  | [] -> raise Not_found
  | x :: rest -> m x 1 0 rest

(* Resize a rectangle object *)
let rect_resize w =
  let obj = ref None
  and tlx = ref 0 and tly = ref 0
  and brx = ref 0 and bry = ref 0 
  and idx = ref 0
  and rx = ref (ref 0) and ry = ref (ref 0) in
  let mark ei =
    (* we must convert x/y into real canvas coords, since we might be
       viewing a subpart of the canvas *)
    let cx = truncate (Canvas.canvasx w (Pixels ei.ev_MouseX))
    and cy = truncate (Canvas.canvasy w (Pixels ei.ev_MouseY)) in
    match Canvas.find w [Closest (Pixels cx, Pixels cy)] with
    | [] -> obj := None
    | i :: _ -> 
      	obj := Some i;
	(* What corner of the rectangle ? *)
        match List.map truncate (Canvas.coords_get w i) with
   	| [x; y; x'; y'] ->
            tlx := x; tly := y; brx := x'; bry := y';
            let dists =
              List.map (distance (cx,cy)) [x, y; x', y; x', y'; x, y'] in
            begin match min_index dists with
            | 0 -> rx := tlx; ry := tly
            | 1 -> rx := brx; ry := tly
            | 2 -> rx := brx; ry := bry
            | 3 -> rx := tlx; ry := bry
            | _ -> assert false end
        | _ -> assert false
  and drag ei =
    match !obj with
    | None -> ()
    | Some i ->
      let cx = truncate (Canvas.canvasx w (Pixels ei.ev_MouseX))
      and cy = truncate (Canvas.canvasy w (Pixels ei.ev_MouseY)) in
      let dx = cx - !(!rx)
      and dy = cy - !(!ry) in
      Canvas.coords_set w i 
        [Pixels !tlx; Pixels !tly; Pixels !brx; Pixels !bry];
      !rx := cx;
      !ry := cy in
   Canvas.bind w (Tag "sizeable")
     [[], ButtonPressDetail 3] (BindSet ([Ev_MouseX; Ev_MouseY], mark));
   Canvas.bind w (Tag "sizeable")
     [[Button3], Motion] (BindSet ([Ev_MouseX; Ev_MouseY], drag))


let init f ctx =
  let width =
    try int_of_string (List.assoc "width" ctx#params)
    with Not_found | Failure "int_of_string" -> 200
  and height =
    try int_of_string (List.assoc "height" ctx#params)
    with Not_found | Failure "int_of_string" -> 200
  in
  let c = 
    Canvas.create f [Width (Pixels (width - 50)); Height (Pixels height);
		     Background White]
  in
  canvas_dragging c;
  rect_resize c;

  let create_rect col () = 
    ignore
      (Canvas.create_rectangle c 
        (Pixels 0) (Pixels 0) (Pixels 50) (Pixels 50)
        [ FillColor (NamedColor col);
          Outline Black; Width (Millimeters 1.0);
          Tags [Tag "movable"; Tag "sizeable"]])
  in
  (* Buttons *)
  let fbuts = Frame.create f [] in
  let buts = List.map
      (fun (txt,col) -> 
      	Button.create fbuts 
          [ Text txt; Command (create_rect col);
            Foreground (NamedColor col); 
	    Background (NamedColor 
			  (if col = "black" then "white" else "black"))])
      [ "Blue", "blue";
       	"Red", "red";
       	"Yellow", "yellow";
       	"Gray", "gray50";
       	"White", "white";
        "Black", "black"] in
  pack buts [Fill Fill_X];
  pack [c][Side Side_Left];
  pack [fbuts][Side Side_Right];
  Frame.configure f [BorderWidth (Pixels 2); Relief Sunken]

let _ = Applets.register  "main" init

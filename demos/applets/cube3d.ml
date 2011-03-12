open Safe418

open Tk
open Viewers

(***** *****)

type vector = float * float * float
type matrix = vector * vector * vector

let x_rotation a = let c = cos a and s = sin a in
    ((1.0, 0.0, 0.0), (0.0, c, s), (0.0, -. s, c))
and y_rotation a = let c = cos a and s = sin a in
    ((c, 0.0, -. s), (0.0, 1.0, 0.0), (s, 0.0, c))

let matrix_vector ((a11, a21, a31), (a12, a22, a32), (a13, a23, a33))
    (x, y, z) =
  (a11 *. x +. a12 *. y +. a13 *. z,
   a21 *. x +. a22 *. y +. a23 *. z,
   a31 *. x +. a32 *. y +. a33 *. z)

(***** *****)

let cx = 2.0 and cy = 2.0
let zoom = (cx +. cy) *. 0.5
let dist = 4.0

let inches l = Pixels (int_of_float (75.0 *. l))

(***** *****)

let viewer w speed =
  let width = inches (2.0 *. cx) in
  let height = inches (2.0 *. cy) in
  let anim_speed = ref speed in
  let c = Canvas.create w [ Width width;
                            Height height ]
  and s = Scale.create w [From 40.0; Length width;
      	       	       	  ShowValue true; TickInterval 30.0;
			  To 250.0; Orient Horizontal;
			  Label "Timer (ms)";
      	       	       	  ScaleCommand 
      	       	       	   (fun f -> anim_speed := truncate f)]
  in
  Scale.set s (float speed);
  pack [c; s] [];

  let points = [|
    (-1.0, -1.0, -1.0); (1.0, -1.0, -1.0); (1.0, 1.0, -1.0); (-1.0, 1.0, -1.0);
    (-1.0, -1.0, 1.0); (1.0, -1.0, 1.0); (1.0, 1.0, 1.0); (-1.0, 1.0, 1.0)
  |] in
  let polys = [
    [0; 3; 2; 1], Red; [0; 1; 5; 4], Green; [1; 2; 6; 5], Blue;
    [2; 3; 7; 6], Yellow; [3; 0; 4; 7], Black; [4; 5; 6; 7], White
  ] in

  (* Keep the same canvas items *)
  let tkpolys =
   let pts = [ Pixels 0; Pixels 0;
	       Pixels 0; Pixels 0;
	       Pixels 0; Pixels 0;
	       Pixels 0; Pixels 0 ] in
    List.map (fun (_, color) -> Canvas.create_polygon c pts [FillColor color])
              polys in
  
  let ax = ref 0.0 and ay = ref 0.0 in
  let xstep = 0.03 and ystep = 0.05 in

  let rect =
    Canvas.create_rectangle c (Pixels 0) (Pixels 0) width width [] in

  let one_step _ =
    ax := !ax +. xstep;
    ay := !ay +. ystep;
    let mx, my = x_rotation !ax, y_rotation !ay in
    let proj v = 
      let (x, y, z) = matrix_vector mx (matrix_vector my v) in
      let z = z +. dist in
      let sx = zoom *. x /. z and sy = zoom *. y /. z in
      [ Inches (cx +. sx); Inches (cy +. sy) ] in
    let trpoints = Array.map proj points in
    let move_poly item (point_list, _) =
      let ppts = List.map (function i -> trpoints.(i)) point_list in
      Canvas.coords_set c item (List.flatten ppts);
      match ppts with
      | [Inches x0; Inches y0] ::
        [Inches x1; Inches y1] ::
        [Inches x2; Inches y2] :: _ ->
          if (x2 -. x0) *. (y1 -. y0)  -.  (x1 -. x0) *. (y2 -. y0) >= 0.0 then
              Canvas.raise_above c item rect
          else Canvas.lower_below c item rect
      | _ -> assert false
    in
    List.iter2 move_poly tkpolys polys

  in
  let running = ref true in
  let rec tim () =
    if Winfo.exists w && Winfo.viewable w then begin
      one_step();
      Timer.set !anim_speed tim
      end 
    else running := false
    in
  bind w [[], Visibility]
    (BindSet([], (fun _ -> if not !running then begin 
      	       	       	     running := true;
      	       	       	     tim()
      	       	       	   end)));

  tim()

let f frame ctx =
  let speed = try int_of_string (List.assoc "delay" ctx#params)
      	      with _ -> 100 in
  viewer frame speed

let _ = Applets.register "main" f

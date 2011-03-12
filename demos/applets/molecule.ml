open Safe418

open Tk
open Viewers

(***** *****)

type vector = float * float * float
type matrix = vector * vector * vector

let x_rotation a = let c = cos a and s = sin a in
    ((1.0, 0.0, 0.0), (0.0, c, s), (0.0, -.s, c))
and y_rotation a = let c = cos a and s = sin a in
    ((c, 0.0, -.s), (0.0, 1.0, 0.0), (s, 0.0, c))

let matrix_vector
    ((a11, a21, a31), (a12, a22, a32), (a13, a23, a33)) (x, y, z) =
  (a11 *. x +. a12 *. y +. a13 *. z,
   a21 *. x +. a22 *. y +. a23 *. z,
   a31 *. x +. a32 *. y +. a33 *. z)

(***** *****)

let cx = 2.0 and cy = 2.0
let zoom = (cx +. cy) *. 0.5
let dist = 5.0

let inches l = Pixels (int_of_float (75.0 *. l))

(***** *****)

let viewer w speed =
  let anim_speed = ref speed in
  let c = Canvas.create w [ Width (inches (2.0 *. cx));
                            Height (inches (2.0 *. cy)) ]
  and s = Scale.create w [From 40.0; Length (inches (2.0 *. cx));
      	       	       	  ShowValue true; TickInterval 30.0;
			  To 250.0; Orient Horizontal;
			  Label "Timer (ms)";
      	       	       	  ScaleCommand 
      	       	       	   (fun f -> anim_speed := truncate f)] in
  Scale.set s (float speed);
  pack [c;s] [];

  let spheres = [
    (-0.8, -0.8, -0.8), 1.0, Black;
    (-3.0, -1.0, -1.0), 0.5, White;
    (-1.0, -3.0, -1.0), 0.5, White;
    (-1.0, -1.0, -3.0), 0.5, White;
    (0.8, 0.8, 0.8), 1.0, Black;
    (3.0, 1.0, 1.0), 0.5, White;
    (1.0, 3.0, 1.0), 0.5, White;
    (1.0, 1.0, 3.0), 0.5, White
  ] in
  
  let display = ref [] in
  let ax = ref 0.0 and ay = ref 0.0 in
  let xstep = 0.03 and ystep = 0.05 in

  let one_step _ =
    Canvas.delete c !display;
    ax := !ax +. xstep;
    ay := !ay +. ystep;
    let mx, my = x_rotation !ax, y_rotation !ay in
    let calc_oval (v, r, col) =
      let (x, y, z) = matrix_vector mx (matrix_vector my v) in
      let z = z +. dist in
      let sx = zoom *. x /. z and sy = zoom *. y /. z in
      let r = zoom *. r /. z in
      ( z, cx +. sx, cy +. sy, r, col )
    in
    let atoms = Sort.list (fun (i, _, _, _, _) (j, _, _, _, _) -> i >= j)
                     (List.map calc_oval spheres) in
    let create_oval (_, x, y, r, col) =
      Canvas.create_oval c (inches (x -. r)) (inches (y -. r))
        (inches (x +. r)) (inches (y +. r)) 
        [ FillColor col ]
    in
    display := List.map create_oval atoms
  in
  let running = ref true in
  let rec tim () =
    if Winfo.exists w & Winfo.viewable w then begin
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

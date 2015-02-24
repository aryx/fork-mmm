(*s: ./extensions/tachy_space.ml *)
open Safe418mmm
open Tk


module Provide = struct
  let capabilities = Capabilities.get()
  end

module Mmm = Get(Provide)


(*s: constant Tachy_space.mpoly_data *)
(* Tachymeter *)

let mpoly_data = 
[5.684359, -36.000000;
14.760086, -36.000000;
19.000000, -20.736308;
23.239914, -36.000000;
31.760086, -36.000000;
36.000000, -20.736308;
40.239914, -36.000000;
46.315641, -36.000000;
37.760086, -5.200000;
30.239914, -5.200000;
26.000000, -20.463692;
21.760086, -5.200000;
14.239914, -5.200000]
(*e: constant Tachy_space.mpoly_data *)

(*s: enum Tachy_space.vector *)
type vector = float * float * float
(*e: enum Tachy_space.vector *)
(*s: enum Tachy_space.matrix *)
type matrix = vector * vector * vector
(*e: enum Tachy_space.matrix *)

let x_rotation a = let c = cos a and s = sin a in
    ((1.0,0.0,0.0), (0.0,c,s), (0.0,-.s,c))
and y_rotation a = let c = cos a and s = sin a in
    ((c,0.0,-.s), (0.0,1.0,0.0), (s,0.0,c))

(*s: function Tachy_space.matrix_vector *)
let matrix_vector ((a11,a21,a31), (a12,a22,a32), (a13,a23,a33)) (x,y,z) =
  (a11*.x+.a12*.y+.a13*.z, a21*.x+.a22*.y+.a23*.z, a31*.x+.a32*.y+.a33*.z)
(*e: function Tachy_space.matrix_vector *)

(*s: constant Tachy_space.pi *)
let pi = 3.1415926 
(*e: constant Tachy_space.pi *)
(*s: constant Tachy_space.log10 *)
let log10 = log 10.0 
(*e: constant Tachy_space.log10 *)

(*s: enum Tachy_space.ball *)
type ball = {
  tag : Tk.tagOrId;
  mutable x : float;
  mutable y : float;
  mutable z : float
 }
(*e: enum Tachy_space.ball *)

class space_tachy (top : Widget.widget) =
 object (self)
  (* val top = top *)
  val mutable fr = top (* dummy initialisation *)
  val mutable c = top (* dummy initialisation *)
  val mutable c2 = top (* dummy initialisation *)
  val mutable mpoly = Tag "none" (* dummy initialisation *)
  val mutable i_park = Tag "none" (* dummy initialisation *)
  val mutable kilos = Tag "none" (* dummy initialisation *)
  val mutable pendings = Tag "none" (* dummy initialisation *)
  val mutable alive = false

  val mutable balls = Array.create 32 {tag = Tag "none"; x = 0.; y = 0.; z = 0.}
  val spacewidth = 4.0

  (* this one is private *)
  method start =
    fr <- Frame.create_named top "tachymeter" [BorderWidth (Pixels 2)];
    c <- Canvas.create fr
      	[ Width (Pixels 72); Height (Pixels 72); 
      	  BorderWidth (Pixels 1);
      Relief Sunken;
      	  HighlightThickness (Pixels 0);
      	  TakeFocus true (* pl3 fix *);
      Background Black];
    c2 <- Canvas.create fr [Width (Pixels 72); Height (Pixels 16)];
    pack [c; c2] [Side Side_Top; Fill Fill_X];

    i_park <- 
       Canvas.create_rectangle c2 
    	 (Pixels 1) (Pixels 1) 
    	 (Pixels 4) (Pixels 4) [FillColor Black];

    kilos <-
       Canvas.create_text c2
      	 (Pixels 36) (Pixels 8)
     [Text "0"; Font "variable"];

    pendings <-
       Canvas.create_text c2
      	 (Pixels 68) (Pixels 8)
     [Text "0"; Font "variable"];

    balls <-
       Array.map (fun _ -> 
     { tag = Canvas.create_line c [Pixels 100; Pixels 100; 
                                  Pixels 100; Pixels 100] 
                                  [FillColor (NamedColor "White")];
       x = Random.float spacewidth -. (spacewidth /. 2.0);
       y = Random.float spacewidth -. (spacewidth /. 2.0);
       z = Random.float 0.9 +. 0.1 }) balls;

    mpoly <- Canvas.create_polygon c (List.fold_right (fun (x,y) s ->  
      let x = truncate ((x -. 26.0) *. 1.3) + 36
      and y = 36 - truncate ((20.6 +. y) *. 1.3) 
      in [Pixels x; Pixels y] @ s) 
                       mpoly_data [])
      	[Width (Pixels 2); FillColor Green; Outline White];

    for i = 0 to Array.length balls - 1 do
      self#ball_update balls.(i) 0.0
    done;
    
    Canvas.lower_bot c2 pendings;

    bind c [[], Destroy] (BindSet ([], (fun _ -> alive <- false)));
    (* These bindings are specific to the applet version *)
    bind c [[], ButtonPressDetail 1] 
      (BindSet ([], (fun _ -> Mmm.new_window_initial (); ())));
    bind c [[], ButtonPressDetail 2] 
      (BindSet ([], (fun _ -> Mmm.new_window_sel (); ())));

    alive <- true;
    pack [fr][]
    
  val mutable mx = 0.
  val mutable my = 0.
  val mutable last_speed = 0.
  val mutable last_speed2 = 0.
  val mutable last_total = 0
  val mutable idle = false

  method ball_update ball speed =
    let x = truncate (ball.x *. (0.2 /. ball.z) *. 32.0) + 36
    and y = truncate (ball.y *. (0.2 /. ball.z) *. 32.0) + 36
    and x' = truncate (ball.x *. (0.2 /. (ball.z +. speed)) *. 32.0) + 36
    and y' = truncate (ball.y *. (0.2 /. (ball.z +. speed)) *. 32.0) + 36
    in
    let x', y' = if (x,y) = (x',y') then x', y'+1 else x', y' in 
    Canvas.coords_set c ball.tag
      [Pixels x; Pixels y; Pixels x'; Pixels y'];
    let x =
      Printf.sprintf "%02X" (truncate ((1.0 -. ball.z) /. (1.0 -. 0.1) *. 255.0)) in
    Canvas.configure_line c ball.tag [FillColor (NamedColor ("#"^x^x^x))]

    
  method update speed total =
     Canvas.coords_set c mpoly 
       (List.fold_right (fun (x,y) s ->  
     let x = (x -. 26.0) *. 1.3
     and y = (20.6 +. y) *. 1.3
         in
     let (x,y,z) = matrix_vector (x_rotation mx) (matrix_vector (y_rotation my) (x,y,0.0)) in
       let r = (z +. 200.0) /. 200.0 in
       let x = truncate ( x *. r ) + 36
       and y = - truncate ( y *. r ) + 36
         in [Pixels x; Pixels y] @ s) mpoly_data []) ;
        mx <- mx +. 0.01;
        my <- my +. 0.02;
      if speed = 0.0 then begin
    if not idle then begin
      Canvas.configure_rectangle c2 i_park [FillColor Black;
                           Outline Black];
      idle <- true
    end
       end
      else begin
    Canvas.configure_rectangle c2 i_park [FillColor Green;
                         Outline Green];
        idle <- false
      end;
       if total <> last_total then
          Canvas.configure_text c2 kilos 
        [Text (if total > 1000000 then 
                 Printf.sprintf "%d.%02dM" (total/1000000)
                                         ((total mod 1000000)/10000)
               else if total > 1000 then
             Printf.sprintf "%d.%01dK" (total/1000)
                                             ((total mod 1000)/100)
           else string_of_int total)];
       last_total <- total;
       let speed = if speed = 0. then 0. else log speed in
       (* Smooth *)
       last_speed2 <-
      if last_speed2 > speed *. 0.8 +. last_speed2 *. 0.2
      then last_speed2 -. 0.1
      else speed;
       let speeds = 
     if last_speed2 -. last_speed > 0.2 then last_speed +. 0.2
     else if last_speed2 -. last_speed < (-0.1) then last_speed -. 0.1
     else last_speed
       in   
       (* let speeds = last_speed *. 0.5 +. speed *. 0.5 in *)
       if abs_float (speeds -. last_speed) > 0.05 then begin
       	 last_speed <- speeds;
     let v = speeds /. log10 *. 0.02 in

     for i = 0 to Array.length balls - 1 do
       balls.(i).z <- balls.(i).z -. v;
       if( balls.(i).z < 0.1 ) then begin
         balls.(i).z <- 1.0;
         balls.(i).x <- Random.float spacewidth -. (spacewidth /. 2.0);
         balls.(i).y <- Random.float spacewidth -. (spacewidth /. 2.0)
       end;
       self#ball_update balls.(i) v;
     done;
         update_idletasks()
        end


  method report_cnx n = 
    if Winfo.exists c2 then
      if n = 0 then begin
    Canvas.configure_text c2 pendings [Text ""];
      	Canvas.lower_bot c2 pendings
      end
      else begin
    Canvas.configure_text c2 pendings 
      [Text (string_of_int n)];
      	Canvas.raise_top c2 pendings
      end

  method report_busy busy =
    if Winfo.exists c2 then
      if busy then begin
      	Canvas.lower_bot c2 pendings;
    Canvas.configure_rectangle c2 i_park [FillColor Red;
                          Outline Red];
    update_idletasks()
      end
      else begin
      	Canvas.raise_top c2 pendings;
    Canvas.configure_rectangle c2 i_park [FillColor Black;
                          Outline Black]
      end

  method report_traffic tick_duration bytes_read sample_read =
    if alive then
      self#update (float sample_read *. 1000. /. float tick_duration)
    bytes_read

  method quit =
    alive <- false;
    destroy fr

end

(*s: function Tachy_space.create_tachy *)
let create_tachy top = 
  let o = new space_tachy top in
  o#start;
(*e: function Tachy_space.create_tachy *)
  (o :> Mmm.tachymeter)

(*s: toplevel Tachy_space._1 *)
let _ = 
  let top = Applets.get_toplevel_widget [] in
  Wm.withdraw top;
  begin match Frx_dialog.f top (Mstring.gensym "foo")
        "Tachy test" "Use the space tachymeter"
    (Tk.Predefined "question") 1 ["Yes"; "No"] with
    0 -> Mmm.set_tachy create_tachy
  | _ -> ()
  end;
  destroy top
(*e: toplevel Tachy_space._1 *)

(*e: ./extensions/tachy_space.ml *)

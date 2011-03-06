open Safe418mmm

open Tk
open Viewers
open Document
open Www


let f fw ctx =
   let c = Canvas.create fw [Width (Pixels 200); Height (Pixels 200)] in
   let create_eye cx cy wx wy ewx ewy bnd =
     let o2 = Canvas.create_oval c 
       (Pixels (cx - wx)) (Pixels (cy - wy)) 
      	 (Pixels (cx + wx)) (Pixels (cy + wy)) 
      	   [Outline (NamedColor "black"); Width (Pixels 7); 
      	     FillColor (NamedColor "white")] 
     and o = Canvas.create_oval c 
       (Pixels (cx - ewx)) (Pixels (cy - ewy)) 
         (Pixels (cx + ewx)) (Pixels (cy + ewy)) 
      	   [FillColor (NamedColor "black")] in
     let curx = ref cx
     and cury = ref cy in
       bind c [[], Motion]
         (BindExtend ([Ev_MouseX; Ev_MouseY], (fun e ->
       	   let nx, ny =
      	     let xdiff = e.ev_MouseX - cx 
	     and ydiff = e.ev_MouseY - cy in
	     let diff = sqrt (((float xdiff) /. ((float wx) *. bnd)) ** 2.0 +. 
               ((float ydiff) /. ((float wy) *. bnd)) ** 2.0) in
	     if diff > 1.0 then
      	       truncate ((float xdiff) *. (1.0 /. diff)) + cx,
	       truncate ((float ydiff) *. (1.0 /. diff)) + cy
	     else 
               e.ev_MouseX, e.ev_MouseY
	 in
	   Canvas.move c o (Pixels (nx - !curx)) (Pixels (ny - !cury));
	   curx := nx;
	   cury := ny)))
  in
     create_eye 60 100 30 40 5 6 0.6;
     create_eye 140 100 30 40 5 6 0.6;
     pack [c] []

let _ = Applets.register "f" f

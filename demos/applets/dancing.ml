open Safe418mmm

open Tk
open Document
open Viewers

let rec cbk c l l_pos = function () ->
  List.iter2 (fun o xyref ->
    let nx = fst !xyref + Random.int 7 - 3
    and ny = snd !xyref + Random.int 7 - 3 in
    let nx = if nx < -10 then -10 else if nx > 10 then 10 else nx
    and ny = if ny < -10 then -10 else if ny > 10 then 10 else ny in
      Canvas.move c o (Pixels (nx - fst !xyref)) (Pixels (ny - snd !xyref));
      xyref := nx, ny) l l_pos;
  Timer.set 100 (cbk c l l_pos)

let f fw ctx =
   let c = Canvas.create fw [Width (Pixels 200); Height (Pixels 200)] in
   let create_letter c x y ch =
     Canvas.create_text c (Pixels x) (Pixels y)
      [Text ch; Font "-*-times-*-r-*-24-*-iso8859-1"] in
   let l = [create_letter c 30 100 "M";
       	    create_letter c 50 100 "M";
	    create_letter c 70 100 "M";
	    create_letter c 103 100 "A";
	    create_letter c 120 100 "p";
	    create_letter c 135 100 "p";
	    create_letter c 147 100 "l"; 
	    create_letter c 159 100 "e"; 
	    create_letter c 172 100 "t"] in
   let l_pos = List.map (fun _ -> ref (0,0)) l  	    
   in
     pack [c] [];
     Timer.set 100 (cbk c l l_pos)

let _ = Applets.register "f" f

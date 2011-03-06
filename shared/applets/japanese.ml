open Safe418mmm

open Tk
open Viewers
open Document
open Www
open Jtk
 
open Kanji

let rec cbk c l l_pos = function () ->
  List.iter2 (fun o xyref ->
    let nx = fst !xyref + Random.int 7 - 3
    and ny = snd !xyref + Random.int 7 - 3 in
    (* trim *)
    let nx = if nx < -10 then -10 else if nx > 10 then 10 else nx
    and ny = if ny < -10 then -10 else if ny > 10 then 10 else ny in
      Canvas.move c o (Pixels (nx - fst !xyref)) (Pixels (ny - snd !xyref));
      xyref := nx, ny) l l_pos;
  Timer.set 100 (cbk c l l_pos)

let f fw ctx = 
   let c = Canvas.create fw [Width (Pixels 200); Height (Pixels 200)] in
   let create_letter c x y ch = 
     let txt = Canvas.create_text c (Pixels x) (Pixels y) [Text ch] in
     Kanji.canvas_item c txt
       [KanjiFont "-jis-fixed-medium-r-normal-*-24-*-*-*-*-*-jisx0208.1983-0"];
     txt  
   in
   let l = [create_letter c 20 100 "も";
       	    create_letter c 44 100 "〜";
	    create_letter c 80 100 "あ";
	    create_letter c 104 100 "ぷ";
	    create_letter c 128 100 "れ";
	    create_letter c 152 100 "っ";
	    create_letter c 176 100 "と" ] in 
   let l_pos = List.map (fun _ -> ref (0,0)) l  	    
   in 	    
     pack [c] [];
     Timer.set 100 (cbk c l l_pos)

let _ = Applets.register "f" f

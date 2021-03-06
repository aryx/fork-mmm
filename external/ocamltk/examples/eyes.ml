(*************************************************************************)
(*                                                                       *)
(*                Objective Caml CamlTk library                          *)
(*                                                                       *)
(*         Jun Furuse, projet Cristal, INRIA Rocquencourt                *)
(*        Pierre Weis, projet Cristal, INRIA Rocquencourt                *)
(*                                                                       *)
(*   Copyright 2000 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.                               *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* The eyes of Caml (CamlTk) *)

open Tk;;

let _ =
  (* parse argv using Protocol.keywords *)
  Arg.parse keywords (fun _ -> ()) "eyes";
  (* then opentk with the result *)
  let top = opentk_with_parsed_args () in

  let fw = Frame.create top [] in
  pack [fw] [];
  let c = Canvas.create fw [Width (Pixels 200); Height (Pixels 200)] in
  let create_eye cx cy wx wy ewx ewy bnd =
    let o2 =
       Canvas.create_oval c
        (Pixels (cx - wx)) (Pixels (cy - wy))
        (Pixels (cx + wx)) (Pixels (cy + wy))
        [Outline (NamedColor "black"); Width (Pixels 7);
         FillColor (NamedColor "white")]
    and o =
      Canvas.create_oval c
       (Pixels (cx - ewx)) (Pixels (cy - ewy))
       (Pixels (cx + ewx)) (Pixels (cy + ewy))
       [FillColor (NamedColor "black")] in
    let curx = ref cx
    and cury = ref cy in
    bind c [[], Motion]
      (BindExtend ([Ev_MouseX; Ev_MouseY],
        (fun e ->
          let nx, ny =
            let xdiff = e.ev_MouseX - cx 
            and ydiff = e.ev_MouseY - cy in
            let diff = sqrt ((float xdiff /. (float wx *. bnd)) ** 2.0 +. 
                               (float ydiff /. (float wy *. bnd)) ** 2.0) in
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

let _ = Printexc.print mainLoop ()





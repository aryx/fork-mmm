(*s: ./retrieve/progress.ml *)
open Tk

(* Progress meter
 * JPF + cleanup by FRX with lazy
 *)
let okcolor = NamedColor "#3cb371"
and kocolor = NamedColor "#dc5c5c"

(*s: constant Progress.no_meter *)
let no_meter = (fun _ _ -> () : Scheduler.progress_func)
(*e: constant Progress.no_meter *)

(*s: function Progress.meter *)
let meter frame =
  (* expected name of "alt" widget *)
  let altw = Widget.atom frame "alt" in
  if Winfo.exists altw then
    (* the progress widget is created on the first call *)
    let bw = lazy (int_of_string (cget frame CBorderWidth)) in
    let width = lazy (Winfo.reqwidth frame - Lazy.force bw * 2)
    and height = lazy (Winfo.reqheight frame - Lazy.force bw * 2) in
    let canv = 
      lazy (
      let c = 
    Canvas.create_named frame "gauge" [ 
    Width (Pixels (Lazy.force width - 2)); (* a kind of magic... *)
    Height (Pixels 3);
    BorderWidth (Pixels 1);
    Relief Sunken ]
      in
      place c [X (Pixels 0); Y (Pixels (Lazy.force height)); Anchor SW];
      c)
    in
    let rect = lazy
      (Canvas.create_rectangle (Lazy.force canv)
     (Pixels 0) (Pixels 0) (Pixels 0) (Pixels 0) 
     [ FillColor okcolor; Outline okcolor])
    in
    let rotate = ref 0 in
    (fun total cur ->
      (* always check that alt widget is still there *)
      if Winfo.exists altw then
    if cur = -1 then
      Canvas.configure_rectangle (Lazy.force canv) (Lazy.force rect)
        [ FillColor kocolor; Outline kocolor]
    else match total with
    | Some cont -> (* expected length is known *)
        let x = 
          if cont = 0 then (Lazy.force width - 2)
          else cur * (Lazy.force width - 2) / cont + 1 in
        Canvas.coords_set (Lazy.force canv) (Lazy.force rect)
          [ Pixels 1; Pixels 1; Pixels x; Pixels 4 ]
    | None -> (* expected length is unknown *)
        if cur <> 0 then begin
          let w = Lazy.force width / 16 in
          Canvas.coords_set (Lazy.force canv) (Lazy.force rect)
           [ Pixels (1 + w * !rotate); Pixels 1; 
          Pixels (1 + w * (!rotate + 2)); Pixels 4 ];
          rotate := (!rotate + 1) mod 15
        end)
  else
    no_meter
(*e: function Progress.meter *)
(*e: ./retrieve/progress.ml *)

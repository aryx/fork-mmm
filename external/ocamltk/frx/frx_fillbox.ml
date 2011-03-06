open Tk

(*
 * Progress indicators
 *)
let okcolor = NamedColor "#3cb371"
and kocolor = NamedColor "#dc5c5c"


let new_vertical parent w h =
  let c = Canvas.create_named parent "fillbox"
            [Width (Pixels w); Height (Pixels h); BorderWidth (Pixels 1);
	     Relief Sunken]
  in
  let i = Canvas.create_rectangle c (Pixels 0) (Pixels 0) (Pixels w) (Pixels 0)
            [FillColor okcolor; Outline okcolor]
  in
    c, (function
	   0 -> Canvas.configure_rectangle c i [FillColor okcolor; 
						Outline okcolor]; 
	        Canvas.coords_set c i [Pixels 0; Pixels 0;
					Pixels w; Pixels 0]
         | -1 -> Canvas.configure_rectangle c i [FillColor kocolor;
						 Outline kocolor]
	 | n ->
	     let percent = if n > 100 then 100 else n in
	     let hf = percent*h/100 in
	       Canvas.coords_set c i [Pixels 0; Pixels 0;
				       Pixels w; Pixels hf])

let new_horizontal parent w h =
  let c = Canvas.create_named parent "fillbox"
            [Width (Pixels w); Height (Pixels h); BorderWidth (Pixels 1);
	     Relief Sunken]
  in
  let i = Canvas.create_rectangle c (Pixels 0) (Pixels 0) (Pixels 0) (Pixels h)
            [FillColor okcolor; Outline okcolor]
  in
    c, (function
	   0 -> Canvas.configure_rectangle c i [FillColor okcolor; 
						Outline okcolor]; 
	        Canvas.coords_set c i [Pixels 0; Pixels 0;
					Pixels 0; Pixels h]
         | -1 -> Canvas.configure_rectangle c i [FillColor kocolor;
						 Outline kocolor]
	 | n ->
	     let percent = if n > 100 then 100 else n in
	     let wf = percent*w/100 in
	       Canvas.coords_set c i [Pixels 0; Pixels 0;
				       Pixels wf; Pixels h])

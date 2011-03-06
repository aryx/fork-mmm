open Safe418
open Protocol
open Tk
open Tkanim

open Morpher
module Device = Pixmap
module MorphTk = Morpher.Make(Device)

open Hyper
open Viewers

module Provide = struct
  let capabilities = Capabilities.get()
  end

module Net = Retrieval(Provide)
open Net

let initialise top url =
  let fr = Frame.create top [] in
  let c = Canvas.create fr [Width (Pixels 320); Height (Pixels 240)] in
  let f2 = Frame.create top [] in
  let en = Entry.create f2 [TextWidth 80] in
  let mb = Menubutton.create f2 [Text "Images"] in
  let m = Menu.create mb [] in

  Entry.insert en End url;
  Menubutton.configure mb [Menu m];
  pack [en][Side Side_Left; Fill Fill_X];
  pack [mb][Side Side_Right];
  pack [c;f2][Side Side_Top; Fill Fill_X];
  pack [fr][];

  let original_image = ref (Device.create 1 1)
  and current_image = ref (Device.create 1 1)
  and warped_image = ref (Device.create 1 1)

  and item = ref (Tag "none")
  and displayed_img = ref (Imagephoto.create [])

  in

  (* Morph vector origin *)
  let o = ref { x = 0.0 ; y = 0.0 } in

  (* s sampling *)
  let oldx = ref 0
  and oldy = ref 0
  and mindist = 5
  in

  (* Set the morph vector origin *)
  let mark ei =
    o := { x = float ei.ev_MouseX; y = float ei.ev_MouseY};
    oldx := ei.ev_MouseX;
    oldy := ei.ev_MouseY
  in
  (* Compute morph with current vector destination *)
  let move ei =
    (* prerr_endline (string_of_int ei.ev_Time); *)
    if abs (!oldx - ei.ev_MouseX) < mindist 
    && abs (!oldy - ei.ev_MouseY) < mindist then ()
    else begin
      oldx := ei.ev_MouseX;
      oldy := ei.ev_MouseY;
      MorphTk.morph !current_image !warped_image
                    !o { x = float ei.ev_MouseX; y = float ei.ev_MouseY};
      (* prerr_endline "finished morphing"; *)
      Pixmap.set !displayed_img !warped_image;
      (* prerr_endline "finished sync"; *)
    end
  in
  (* Commit current image *)
  let commit _ = 
    Device.copy !warped_image !current_image
  in
  (* Reset to initial image *)
  let reset _ =
    Device.copy !original_image !current_image;
    Pixmap.set !displayed_img !current_image
  in
  Canvas.configure c [Cursor (XCursor "top_left_arrow")];
  Canvas.bind c (Tag "morphed")
    [[], ButtonPressDetail 1] (BindSet([Ev_MouseX; Ev_MouseY], mark));
  Canvas.bind c (Tag "morphed")
    [[Button1], Motion] (BindSet([Ev_MouseX; Ev_MouseY; Ev_Time], move));
  Canvas.bind c (Tag "morphed")
    [[], ButtonReleaseDetail 1] (BindSet([], commit));
  Canvas.bind c (Tag "morphed")
    [[], ButtonPressDetail 2] (BindSet([], reset));

  pack [c][];

  (* Continuation of a photo load *)
  let start img = 
    let w = Imagephoto.width img
    and h = Imagephoto.height img 
    in
    displayed_img := img;
    original_image := Device.create w h;
    current_image := Device.create w h;
    warped_image := Device.create w h;

    Device.copy (Pixmap.get img) !original_image;
    Device.copy !original_image !current_image;
    Device.copy !original_image !warped_image;

    (* destroy previous item *)
    begin try Canvas.delete c [!item]
          with TkError _ -> () end;

    item := 
       Canvas.create_image c (Pixels 0) (Pixels 0)
	 [ImagePhoto img; Anchor NW; Tags [Tag "morphed"]]

  in
  (* Run for an URL *)
  let run url =
   get_image {h_uri = url; h_context = None; h_method = GET; h_params = []}
    (fun _ i ->
      match i with
	Still (ImagePhoto o) -> if Winfo.exists top then begin
	  Entry.delete_range en (Number 0) End;
	  Entry.insert en End url;
	  start o
	end
	 | _ -> ())
  in
  bind en [[], KeyPressDetail "Return"] 
    (BindSet ([], fun _ ->
      let url = Entry.get en in
      Menu.add_command m [Label url; Command (fun () -> run url)];
      run url));
  Menu.add_command m [Label url; Command (fun () -> run url)];
  run

let main top ctx =

  let imgsrc = 
    try List.assoc "url" ctx#params
    with Not_found ->
      "http://pauillac.inria.fr/~rouaix/mecouleur8.gif"
  in
  let run =
  try
    let display = List.assoc "display" ctx#params in
    let top = Applets.get_toplevel_widget [] in
    Wm.withdraw top;
    let top = Toplevel.create top [Screen display] in
    initialise top imgsrc
  with
    Not_found -> initialise top imgsrc in
  run imgsrc

  
let _ = 
  Applets.register "main" main

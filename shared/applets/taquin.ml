open Safe418

open Tk
open Tkanim
open Viewers
open Hyper
open Capabilities
open Document

module Provide = struct
  let capabilities = Capabilities.get()
  end

module Net = Retrieval(Provide)
open Net

(* Produce a permutation of tiles *)
let shuffle l =
  let v = Array.of_list l in
  for i = Array.length v - 1 downto 0 do
    let j = Random.int (i+1) in
      let el = v.(i) in
      	v.(i) <- v.(j);
      	v.(j) <- el
    done;
  Array.to_list v


(* make sure we have reasonable sizes *)
let reasonable size split =
  let tilesize = size / split in
  if tilesize < 10 then
    (size + 9) / 10 , 10
  else split , tilesize (* keep the original values *)

let init top img nx ny =
  let w = Imagephoto.width img
  and h = Imagephoto.height img in
  let c = Canvas.create top [Width (Pixels w); Height (Pixels h)] in
  let nx, tx = reasonable w nx
  and ny, ty = reasonable h ny
  in
  (* Create tiles from picture *)
  let tiles = ref [] in
  for i = 0 to nx - 1 do
    for j = 0 to ny - 1 do
      let tile = Imagephoto.create [Width (Pixels tx); Height (Pixels ty)] in
      Imagephoto.copy tile img [ImgFrom(i * tx, j * ty, (i+1)*tx, (j+1)*ty)];
      tiles := tile :: !tiles
    done
  done;
  (* The blank one is the last one, that is the first one in the list *)
  let blank = List.hd !tiles in
  Imagephoto.blank blank;
  tiles := shuffle !tiles;
  (* Board *)
  let board = Array.create_matrix nx ny (Id 0) in

  let iblank = ref 0
  and jblank = ref 0
  and l = ref !tiles in
  for i = 0 to nx - 1 do
    for j = 0 to ny - 1 do
      match !l with
	[] -> ()
      | x::rest ->
	  if x == blank then begin
	    iblank := i; jblank := j
	  end;
	  board.(i).(j) <-
	     Canvas.create_image c (Pixels (i * tx)) (Pixels (j * ty))
		[ImagePhoto x; Anchor NW; Tags [Tag "tile"]];
	  l := rest
    done
  done;

  let move_tile tile i j =
    Canvas.coords_set c tile [Pixels (i * tx); Pixels (j * ty)];
    board.(i).(j) <- tile
  in
  let play ei =
    let x = ei.ev_MouseX / tx
    and y = ei.ev_MouseY / ty in
    let blankitem = board.(!iblank).(!jblank) in
      if x = !iblank then begin
      	if y > !jblank then
	  for j = !jblank + 1 to y do
	    move_tile board.(x).(j) x (j - 1)
	  done
	else
	 for j = !jblank - 1 downto y do
	   move_tile board.(x).(j) x (j + 1)
	 done;
    	move_tile blankitem x y;
	iblank := x; jblank := y
      end else if y = !jblank then begin
      	if x > !iblank then
	 for i = !iblank + 1 to x do
	   move_tile board.(i).(y) (i - 1) y
	 done
        else
	 for i = !iblank - 1 downto x do
	   move_tile board.(i).(y) (i + 1) y
	 done;
    	move_tile blankitem x y;
	iblank := x; jblank := y
      end  
  in
  Canvas.bind c (Tag "tile") [[], ButtonPressDetail 1]
    (BindSet ([Ev_MouseX; Ev_MouseY], play));
  bind c [[], Destroy]
    (BindSet ([], (fun _ -> List.iter Imagephoto.delete !tiles)));
  c

let f top ctx =
 let nx = 
   try int_of_string (List.assoc "x" ctx#params)
   with Not_found | Failure "int_of_string" ->
     failwith "I need a PARAM X (integer)"
 and ny =
   try int_of_string (List.assoc "y" ctx#params)
   with Not_found | Failure "int_of_string" ->
     failwith "I need a PARAM Y (integer)"
 and width =
   try int_of_string (List.assoc "width" ctx#params)
   with Not_found | Failure "int_of_string" -> 640
 and height =
   try int_of_string (List.assoc "height" ctx#params)
   with Not_found | Failure "int_of_string" -> 480
 and url = try List.assoc "url" ctx#params
       	  with _ -> failwith "I need a PARAM URL (URL)"
 in
 let frc = Frame.create top [] in
 (* the entry for URL *)
 let f2 = Frame.create top [] in
 let en = Entry.create f2 [TextWidth 80] in
 let mb = Menubutton.create f2 [Text "Images"] in
 let m = Menu.create mb [] in

 Entry.insert en End url;
 Menubutton.configure mb [Menu m];
 pack [en][Side Side_Left; Fill Fill_X];
 pack [mb][Side Side_Right];
 pack [f2;frc][Side Side_Top; Fill Fill_X];
 
 let base = Url.string_of ctx#base.document_url in
 let run url =
   get_image {h_uri = url; h_context = Some base;
	      h_method = GET; h_params = []}
     (fun url i ->
       match i with
       | Still (ImagePhoto img) ->
	   (* fix the entry/menu *)
	   Entry.delete_range en (Number 0) End;
	   Entry.insert en End (Url.string_of url);
	   ctx#log "Initialising taquin";
	   List.iter destroy (Winfo.children frc);
      	   pack [init frc img nx ny] [];
	   ctx#log "Enjoy"
       | _ -> failwith "I need an ImagePhoto")
 in
  bind en [[], KeyPressDetail "Return"] 
    (BindSet ([], fun _ ->
      let url = Entry.get en in
      Menu.add_command m [Label url; Command (fun () -> run url)];
      run url));
 Menu.add_command m [Label url; Command (fun () -> run url)];
 run url

let _ = Applets.register  "main" f

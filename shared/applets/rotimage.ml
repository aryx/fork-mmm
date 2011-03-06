open Safe418mmm

open Tkanim
open Tk
open Viewers
open Hyper

open Capabilities
module Provide = struct
  let capabilities = Capabilities.get()
  end

module Net = Retrieval(Provide)
open Net

let timer_list_ref = (ref [] : Timer.t list ref)
(* I know, this should be timer ref, but I'm not sure what should be 
   the initial value ... *)

let remove_timer () =
  match !timer_list_ref with
    [] -> ()
  | [t] -> Timer.remove t (* ; prerr_endline "removed!" *)
  | _ -> raise (Failure "Multi timers !?")  

let update_banner c ci ci' w speed delay = fun () ->
  let rec update x = fun () ->
    let nx = x + speed in
    let nx = if nx > w then nx mod w else nx
    in
      Canvas.move c ci (Pixels (nx - x)) (Pixels 0);
      Canvas.move c ci' (Pixels (nx - x)) (Pixels 0);
      timer_list_ref := [Timer.add delay (update nx)]; (* don't mod nx *)
(*      prerr_endline "update!"; *)
      ()   
  in
    update 0 ()

let f fw ctx =
 let url = try List.assoc "url" ctx#params
       	  with
	    _ -> failwith "I need a PARAM URL (URL)"
 and speed = try int_of_string (List.assoc "speed" ctx#params) with _ -> 1 
 and delay = try int_of_string (List.assoc "delay" ctx#params) with _ -> 100 in
 if speed <= 0 then failwith "Speed must be positive integer (dots)";
 if delay <= 0 then failwith "Delay must be positive integer (mili seconds)";
     get_image
       { h_uri= url;
	 h_context= None;
	 h_method= GET;
	 h_params= []}
       (fun url image ->
	 match image with
       	 Still (ImagePhoto img) ->
	   let w = Imagephoto.width img
	   and h = Imagephoto.height img in
	   (if speed > w then failwith "Too much speed !!!");
	   let c = Canvas.create fw [Width (Pixels w); Height (Pixels h)] in
	     (* stop timer if destroyed *)
      	     bind c [[], Unmap] 
	       (BindSet ([], (fun _ -> remove_timer ())));
	     let ci = Canvas.create_image c (Pixels 0) (Pixels 0) [Anchor NW; ImagePhoto img]
	     and ci' = Canvas.create_image c (Pixels (-w)) (Pixels 0) [Anchor NW; ImagePhoto img] in
	     pack [c][];
      	     timer_list_ref := [Timer.add delay (update_banner c ci ci' 
      	       w speed delay)]; 
      	     ()
       | _ -> failwith "I need an ImagePhoto")

let _ = Applets.register "f" f

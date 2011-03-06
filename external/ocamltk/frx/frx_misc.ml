(* Delayed global, a.k.a cache&carry *)
let autodef f =
  let v = ref None in
  (function () ->
     match !v with
       None ->
       	 let x = f() in
       	   v := Some x;
	   x
     | Some x -> x)

open Tk

(* allows Data in options *)
let create_photo options =
  let hasopt = ref None in
  (* Check options *)
  List.iter (function
      Data s -> 
	begin match !hasopt with
	  None -> hasopt := Some (Data s)
	| Some _ -> raise (Protocol.TkError "two data sources in options")
	end
    | File f -> 
	begin match !hasopt with
	  None -> hasopt := Some (File f)
	| Some _ -> raise (Protocol.TkError "two data sources in options")
	end
    | o -> ())
    options;
  match !hasopt with
    None -> raise (Protocol.TkError "no data source in options")
  | Some (Data s) ->
      begin
      	let tmpfile = Filename.temp_file "img" "" in
      	let oc = open_out_bin tmpfile in
      	output_string oc s;
      	close_out oc;
      	let newopts = 
	  List.map (function 
	    | Data s -> File tmpfile
	    | o -> o)
	    options in
      	try
	  let i = Imagephoto.create newopts in
	  (try Sys.remove tmpfile with Sys_error _ -> ());
	  i
      	with
	  e ->
	    (try Sys.remove tmpfile with Sys_error _ -> ());
	    raise e
      end
  | Some (File s) -> Imagephoto.create options
  | _ -> assert false

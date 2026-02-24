(* This module of the applet system is specific to MMM *)

open Tk
open Prefs

(* Pref stuff *)
let active = ref false

let types = [
  "application","x-caml-applet"
] 

let activate () =
  List.iter (fun ctype -> 
    Viewers.add_viewer ctype Appview.code_viewer;
    Embed.add_viewer ctype Appview.applet_viewer)
  types

let deactivate () =
  List.iter (fun ctype ->
    Viewers.rem_viewer ctype;
    Embed.rem_viewer ctype)
  types

(* set display from prefs *)
let pref_init v =
  Textvariable.set v (if !active then "1" else "0")

let pref_set v =
  match Textvariable.get v with
    "1" -> if !active then () else begin
             active := true;
             activate()
            end
  | _ -> if not !active then () else begin
           active := false;
           deactivate()
         end

(* Preference panel for applets *)
let applets_pref top =
  family top (I18n.sprintf "Applets") [
    abstract_bool_pref "Active" pref_init pref_set;
    bool_pref "Paranoid" Dload.paranoid
    ]


(* Load "initial modules" residing in user directory (~/.mmm) *)
let load_initial_modules () =
  try
    let dir = 
      Filename.concat (Filename.concat (Sys.getenv "HOME") ".mmm") 
                      (string_of_int Version.number) in
    if Sys.file_exists dir then
    let dh = Unix.opendir dir in
    try
      while true do
	let f = Unix.readdir dh in
	if Filename.check_suffix f ".cmo" then
	  Dload.load_local (Filename.concat dir f)
      done
    with
        End_of_file -> Unix.closedir dh
     |  e -> Unix.closedir dh; raise e
  with
  | Not_found (* Sys.getenv *) ->
      prerr_endline "Please specify the HOME environment variable";
      exit (-1)
  | Unix.Unix_error (e, fname, arg) ->
      Error.f (I18n.sprintf
		 "Error during loading of initial modules\n%s: %s %s"
		 fname (Unix.error_message e) arg)

let init initialp =
  Logs.info (fun m -> m "Loading applet system");
  (* Dynamic linking init : both common applets and specific applets *)
  (* old: Dynlink.init();
  Dynlink.add_available_units Crcs.crc_unit_list;
  Dynlink.add_available_units Crcsmmm.crc_unit_list;
  TODO: use instead Dynlink.set_allowed_units
  *)
(* pad: TODO, infer using `ocamlc -where`
  Dynlink.add_interfaces ["Pervasives"; "Unix"] 
    ["/opt/local/lib/ocaml"];
*)
  (* Load local applets *)
  if initialp then load_initial_modules();
  Mmmprefs.plug_applets applets_pref

let _ = Version.applet_init := init

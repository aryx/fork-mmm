(*s: appsys.ml *)
(* This module of the applet system is specific to MMM *)

(*s: constant [[Appsys.active]] *)
(* Pref stuff *)
let active = ref false
(*e: constant [[Appsys.active]] *)

(*s: constant [[Appsys.types]] *)
let types = [
  "application","x-caml-applet"
] 
(*e: constant [[Appsys.types]] *)

(*s: function [[Appsys.activate]] *)
let activate () =
  types |> List.iter (fun ctype -> 
    Viewers.add_viewer ctype Appview.viewer;
    Embed.add_viewer ctype Appview.embed_viewer
  )
(*e: function [[Appsys.activate]] *)
(*s: function [[Appsys.deactivate]] *)
let deactivate () =
  types |> List.iter (fun ctype ->
    Viewers.rem_viewer ctype;
    Embed.rem_viewer ctype
  )
(*e: function [[Appsys.deactivate]] *)

(*s: function [[Appsys.pref_init]] *)
(* set display from prefs *)
let pref_init v =
  Textvariable.set v (if !active then "1" else "0")
(*e: function [[Appsys.pref_init]] *)
(*s: function [[Appsys.pref_set]] *)
let pref_set v =
  match Textvariable.get v with
  | "1" -> if !active then () else begin
             active := true;
             activate()
            end
  | _ -> if not !active then () else begin
           active := false;
           deactivate()
         end
(*e: function [[Appsys.pref_set]] *)

(*s: function [[Appsys.applets_pref]] *)
(* Preference panel for applets *)
let applets_pref top =
  Prefs.family top (I18n.sprintf "Applets") [
    Prefs.abstract_bool_pref "Active" pref_init pref_set;
      (*s: [[Appsys.applets_pref]] other elements *)
      Prefs.bool_pref "Paranoid" Dload.paranoid
      (*e: [[Appsys.applets_pref]] other elements *)
    ]
(*e: function [[Appsys.applets_pref]] *)

(*s: function [[Appsys.load_initial_modules]] *)
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
     | End_of_file -> Unix.closedir dh
     | e -> Unix.closedir dh; raise e
  with
  | Not_found (* Sys.getenv *) ->
      prerr_endline "Please specify the HOME environment variable";
      raise (Exit.ExitCode (-1))
  | Unix.Unix_error (e, fname, arg) ->
      Error.f (I18n.sprintf
         "Error during loading of initial modules\n%s: %s %s"
         fname (Unix.error_message e) arg)
(*e: function [[Appsys.load_initial_modules]] *)

(*s: function [[Appsys.init]] *)
(* Dynamic linking init : both common applets and specific applets *)
let init initialp =
  Logs.info (fun m -> m "Loading applet system");
  (*s: [[Appsys.init()]] sandbox restrictions *)
  (* old: modern OCaml does not need anymore to use this old API
   *  Dynlink.init();
   *  Dynlink.add_available_units Crcs.crc_unit_list;
   *  Dynlink.add_available_units Crcsmmm.crc_unit_list;
  *)
  (* TODO: use instead Dynlink.set_allowed_units ? 
   * TODO: allow_only does not seem to work :(
   *)
  Dynlink.allow_only ["Safe419"];
  (* old? needed? pad: TODO, infer using `ocamlc -where`
     Dynlink.add_interfaces ["Pervasives"; "Unix"] 
      ["/opt/local/lib/ocaml"];
  *)
  (*e: [[Appsys.init()]] sandbox restrictions *)
  (*s: [[Appsys.init()]] load local applets *)
  (* Load local applets *)
  if initialp then load_initial_modules();
  (*e: [[Appsys.init()]] load local applets *)
  (*s: [[Appsys.init()]] plug mmm applets preferences *)
  Mmmprefs.plug_applets applets_pref
  (*e: [[Appsys.init()]] plug mmm applets preferences *)
(*e: function [[Appsys.init]] *)
(*e: appsys.ml *)

(* misc functions *)

let do_nothing = fun _ -> ()

let default f d = try f () with _ -> d

(* we expect HOME to be defined... *)
let user_file name =
  Filename.concat (Sys.getenv "HOME") name


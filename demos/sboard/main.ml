open Tk
open Tree
open Unix

let main () =
  (* As always, we must parse argument first, using references... *)
  let display = ref (try Sys.getenv("DISPLAY") with Not_found -> "")
  and importfiles = ref []
  in
  Arg.parse [
    "-d", Arg.String (fun s -> display := s),
      "<foo:0>\t\tDisplay";
    "-display", Arg.String (fun s -> display := s),
      "<foo:0>\tDisplay"
     ]
    (fun s -> importfiles := s :: !importfiles) 
    "Usage: surfboard <opts> <importfile>";

  if !importfiles = [] then 
    importfiles := [Misc.user_file ".mmm/surfboard.html"];

  Sys.catch_break true;
  (* Avoid SIGPIPE completely, in favor of write() errors *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  
  let t = openTkDisplayClass !display "Surfboard" in

  (* I like variable font *)
  Resource.add "*font" "variable" WidgetDefault;

  Wm.withdraw t;

  Lang.japan := Jtk.is_japanese_mode () || Lang.is_japanese ();

  let tree = ref None in
  (* currently only one file *)
  begin try
    let file = List.hd !importfiles in
    let size = (Unix.stat file).st_size in
    let ic = open_in file in
    tree := Some (Tree.parse (Lexing.from_channel ic));
    close_in ic
  with
    _ -> ()
  end;
  let tree = 
    match !tree with 
      Some t -> t 
    | None -> 
	let init_board = "<TITLE>Bookmarks</TITLE>
<H1>Bookmarks</H1>
<DL>
  <DT><A HREF=\"http://pauillac.inria.fr/~rouaix/mmm\">MMM Home page</A>
</DL>
"
	in
	Tree.parse (Lexing.from_string init_board)
	(* let's hope no error happens *)  
  in

  Addpage.init_external tree;

  Treeview.create tree;

  mainLoop ()

let _ = Printexc.catch main ()

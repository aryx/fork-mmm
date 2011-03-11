open Tk

let top = openTk ()

let scroll_link sb tx =
  Text.configure tx [YScrollCommand (Scrollbar.set sb)];
  Scrollbar.configure sb [ScrollCommand (Text.yview tx)]

let f = Frame.create top []
let text = Text.create f []
let scrollbar = Scrollbar.create f []

(* kill buffer *)
let buffer = ref ""

(* Note: for the text widgets, the insertion cursor is 
    not TextIndex(Insert, []),
    but TextIndex(Mark  "insert", []) 
*) 
let insertMark = TextIndex(Mark "insert", [])
let eol_insertMark = TextIndex(Mark "insert", [LineEnd])

let kill () =
  buffer := 
     Text.get text insertMark eol_insertMark;
  prerr_endline ("Killed: " ^ !buffer);
  Text.delete text insertMark eol_insertMark
;;

let yank () =
  Text.insert text insertMark !buffer [];
  prerr_endline ("Yanked: " ^ !buffer)
;;

let _ =
  scroll_link scrollbar text;

  pack [text; scrollbar][Side Side_Left; Fill Fill_Y];
  pack [f][];

  bind text [[Control], KeyPressDetail "y"]
   (BindSet ([], fun _ -> yank () ));
  bind text [[Control], KeyPressDetail "k"]
   (BindSet ([], fun _ -> kill () ));

  mainLoop ()
;;


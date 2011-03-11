open Tk

let top = openTk ()

let scroll_link sb tx =
  Text.configure tx [YScrollCommand (Scrollbar.set sb)];
  Scrollbar.configure sb [ScrollCommand (Text.yview tx)]

let f = Frame.create top []
let text = Text.create f []
let scrollbar = Scrollbar.create f []

let buffer = ref ""

let kill () =
  buffer := 
     Text.get text (TextIndex (Insert, []))
                   (TextIndex (Insert, [LineEnd]));
     Text.delete text (TextIndex (Insert, []))
                   (TextIndex (Insert, [LineEnd]))
;;

let yank () =
  Text.insert text (TextIndex (Insert, [])) !buffer [] 

let _ = bind text [[Control], KeyPressDetail "y"] (BindSet ([], fun _ ->
  yank () ))
;;
let _ = bind text [[Control], KeyPressDetail "k"] (BindSet ([], fun _ ->
  kill () ))
;;

let _ =
  scroll_link scrollbar text;

  pack [text;f][];
  pack [f][];
  mainLoop ()
;;


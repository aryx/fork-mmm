open Tk;;

let win = openTk();;

let cvs = Canvas.create win [];;

let t = Label.create cvs [Text "File name"];;

let b =
  Button.create cvs
    [Text "Save";
     Command
       (function _ -> 
         let s =
           savefile
             [Title "SAVE FILE TEST";
              DefaultExtension ".foo";
              FileTypes [ { typename= "just test";
                            extensions= [".foo"; ".test"];
                            mactypes= ["FOOO"; "BARR"] } ];
              InitialDir "/tmp";
              InitialFile "hogehoge" ] in
         Label.configure t [Text s])];;

let bb =
  Button.create cvs
    [Text "Open";
     Command
       (function _ ->
          let s = openfile [] in
          Label.configure t [Text s])];;

let q =
  Button.create cvs
    [Text "Quit";
     Command
       (function _ -> closeTk (); exit 0)];;

pack [cvs; q;  bb; b; t] [];;

mainLoop ();;

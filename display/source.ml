(*s: ./display/source.ml *)
open I18n
open Printf
open Tk
open Frx_text
open Document
open Html

(*s: function Source.annotate *)
(* HTML source viewer/editor *)
let annotate txt =
  Hashtbl.iter (fun elem _  ->
    let color = Tkresource.string (sprintf "Source<%s>" elem) "white" in
    Text.tag_configure txt elem [Background (NamedColor color)])
    !Dtd.current.Dtd.contents;
    (fun annotations ->
      List.iter (function (name,Loc(s,e)) ->
    let idxs = abs_index s
    and idxe = abs_index e in
    Text.tag_add txt name idxs idxe)
      annotations)
(*e: function Source.annotate *)

(*s: function Source.view *)
(* Commit modifies the cache *)
let view attach did redisplay errors annotations coding =
  try 
    let doc = Cache.find did in
    (* load : take document from cache and put it in text widget
       commit : take source of text widget and store in cache
       save : save to original URL. supported only on file:, could be
              supported on http: with put ?
     *)
    let load, cachesave, saveurl =
      match doc.document_data with
      | FileData (fname,_) ->
          let tmpfile = Msys.mktemp "buf" in
          (* load *)
          (fun t ->
             let ic = open_in fname in
             let buf = String.create 2048 in
             try
               while true do
                 let n = input ic buf 0 2048 in
                 if n = 0 
                 then raise End_of_file
                 else Text.insert t textEnd 
                        (if n = 2048 then buf else String.sub buf 0 n) []
               done
             with End_of_file -> close_in ic
          ),

         (* commit *)
         (fun t ->
            let oc = open_out tmpfile in
            output_string oc
            (Text.get t (TextIndex(LineChar(0,0), [])) textEnd);
            close_out oc;
            (* SWITCH CACHE *)
            doc.document_data <- FileData(tmpfile, true)),

        (* save *)
        Some (fun t ->
          let oc = open_out fname in
          output_string oc
            (Text.get t (TextIndex(LineChar(0,0), [])) textEnd);
          close_out oc;
          (* SWITCH CACHE *)
          doc.document_data <- FileData(fname, false)
       )

      | MemoryData buf ->
          (* load *)
          (fun t -> Text.insert t textEnd (Ebuffer.get buf) []),
          (* commit *)
          (fun t -> 
             Ebuffer.reset buf;
             Ebuffer.output_string buf
              (Text.get t (TextIndex(LineChar(0,0), [])) textEnd)),
          None
    in
    let top = Toplevel.create attach [Class "MMMSource"] in
    Wm.title_set top "HTML source display";

    let errorv = Textvariable.create_temporary top in
    let f, t = 
      new_scrollable_text top [Foreground Black; Background White] false in
    let f' = Frame.create_named top "buttons" [] in
    let dismiss = Button.create_named f' "dismiss"
      [Text (s_ "Dismiss"); Command (fun _ -> destroy top)] in
    let commit = Button.create_named f' "commit" [Text (s_ "Commit")] in
    let save = Button.create_named f' "save" [Text (s_ "Save")] in
    let err = Button.create_named f' "errors" [] in
    let ferr = Frame.create top [] in
    let err_msg = 
      Label.create_named ferr "error" 
            [Relief Sunken;TextVariable errorv; Anchor W]
  in

  (* Error display and looping *)
  let error_idx = ref [] in
  let get_msg idx =
    let rec f = function
        [] -> raise Not_found
      | (s,e,msg)::l ->
        if Text.compare t s LE idx && Text.compare t idx LE e then msg
     else f l in
    f !error_idx in
  let show_error = Text.yview_index t in
  (* alternative is : Text.see t but is less practical *)
  let loop_in_errors  = 
    let current = ref None in
    (fun () ->
      match !current with
       None -> (* select the first error *)
      let (s,e,_) = List.hd !error_idx in
       current := Some e;
       show_error s
      | Some s -> (* select the next one *)
        try
         let (s,e) = Text.tag_nextrange t "errors" s textEnd in
       current := Some (TextIndex (e,[]));
       show_error (TextIndex(s,[]))
     with _ -> (* no more *)
       let (s,e,_) = List.hd !error_idx in
        current := Some e;
        show_error s) in

  let mark_errors () =
    error_idx := [];
    List.iter (fun (Loc(s,e),msg) ->
      let idxs = abs_index s
      and idxe = abs_index e in
      Text.tag_add t "errors" idxs idxe;
      error_idx := (idxs, idxe, msg) :: !error_idx)
        !errors;
    match List.length !errors with
      0 ->
       Button.configure err [Text (s_ "No Errors"); State Disabled]
    | n ->
    Button.configure err
        [Text (s_ "%d errors" (List.length !errors));
         State Normal; Command loop_in_errors]
  and decorate = annotate t
  in
  let reset () =
    (* if we delete the tag, we delete the bindings. 
     * we can't use the old indexes since the buffer might have changed ! *)
     let rec remall = function
        [] -> ()
      | [x] -> ()
      | s::e::l ->
       Text.tag_remove t "errors" (TextIndex(s,[])) (TextIndex(e,[]));
       remall l
      in
    remall (Text.tag_ranges t "errors");
    errors := [];
    Button.configure err 
       [Text (s_ "Display Errors"); Command mark_errors; State Normal]

  in
     Button.configure commit
        [Command (fun () -> reset(); cachesave t; redisplay())];
     (match saveurl with
        None -> Button.configure save [State Disabled]
      | Some f -> 
          Button.configure save
          [Command (fun () -> reset(); f t; redisplay())]);
     Button.configure err 
       [Text (s_ "Display Errors");
        Command (fun () -> mark_errors(); decorate !annotations);
        State Normal];
     Text.configure t [Background (NamedColor "white")];
     Text.tag_configure t "errors" [Underline true];
     Text.tag_bind t "errors" [[], Enter]
       (BindSet ([Ev_MouseX; Ev_MouseY],
     (fun ei ->
        (* The index of the click position *)
        let i = Text.index t
            (TextIndex (AtXY (ei.ev_MouseX, ei.ev_MouseY), [])) in
          try
        Textvariable.set errorv (get_msg (TextIndex(i,[])))
          with
        Not_found -> ())));
     Text.tag_bind t "errors" [[], Leave]
       (BindSet ([], (fun ei -> Textvariable.set errorv "")));

      pack [dismiss;commit;save;err][Side Side_Left];
      pack [err_msg] [Side Side_Left; Expand true; Fill Fill_X];
    pack [ferr][Side Side_Top; Fill Fill_X];
    pack [f'][Side Side_Top; Fill Fill_X];
    pack [f][Fill Fill_Both; Expand true; Side Side_Bottom];

  Frx_text.addsearch t;
  load t

  with Not_found ->
    Error.f "document not in cache"
(*e: function Source.view *)

(*e: ./display/source.ml *)

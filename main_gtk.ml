(* Yoann Padioleau
 * 
 * Copyright (C) 2015 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

module G = Gui
module Color = Simple_color

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type world = {
  (* viewport, device coordinates *)
  mutable width:  int;
  mutable height: int;
}

(*****************************************************************************)
(* Cairo helpers *)
(*****************************************************************************)

module ArithFloatInfix = struct
    let (+..) = (+)
    let (-..) = (-)
    let (/..) = (/)
    let ( *.. ) = ( * )

    let (+) = (+.)
    let (-) = (-.)
    let (/) = (/.)
    let ( * ) = ( *. )
end
(* floats are the norm in cairo *)
open ArithFloatInfix

(* was in pfff/.../cairo_helpers.ml *)
let set_source_color ?(alpha=1.) ~cr ~color () = 
  (let (r,g,b) = color |> Color.rgbf_of_string in
  Cairo.set_source_rgba cr r g b alpha;
  )


(*let re_space = Str.regexp "^[ ]+$"*)

(* !does side effect on the (mutable) string! *)
let prepare_string s = 
(*  if s ==~ re_space then  s ^ s (* double it *) else  *)
  begin
    for i = 0 to String.length s -.. 1 do
      let c = String.get s i in
      if int_of_char c >= 128
      then String.set s i 'Z'
      else 
        if c = '\t'
        then String.set s i ' '
      else ()
    done;
    s
  end


(* Text cairo API seems buggy, especially on MacOS X, see
 * https://github.com/diagrams/diagrams-cairo/issues/43
 * so I had to hack many things in move_to() by using different extents..
 * Indeed the main cairo documents says Cairo.text_xxx are a "toy text API"
 * http://cairographics.org/manual/cairo-text.html
 *)
(* less: prepare_string thing *)
let show_text cr s =
  (* this 'if' is only for compatibility with old versions of cairo
   * that returns some out_of_memory error when applied to empty strings
   *)
  if s = "" then () else 
  try 
    let s' = prepare_string s in
    Cairo.show_text cr s'
  with _exn ->
    let status = Cairo.status cr in
    let s2 = Cairo.string_of_status status in
    failwith ("Cairo pb: " ^ s2 ^ " s = " ^ s)




let fill_rectangle_xywh ?alpha ~cr ~x ~y ~w ~h ~color () = 
  set_source_color ?alpha ~cr ~color ();
  
  Cairo.move_to cr x y;
  Cairo.line_to cr (x+w) y;
  Cairo.line_to cr (x+w) (y+h);
  Cairo.line_to cr x (y+h);
  Cairo.fill cr;
  ()

let draw_rectangle_xywh ?alpha ~cr ~x ~y ~w ~h ~color () = 
  set_source_color ?alpha ~cr ~color ();
  
  Cairo.move_to cr x y;
  Cairo.line_to cr (x+w) y;
  Cairo.line_to cr (x+w) (y+h);
  Cairo.line_to cr x (y+h);
  Cairo.stroke cr;
  ()

(*****************************************************************************)
(* Draw Efuns API *)
(*****************************************************************************)

(*****************************************************************************)
(* Test cairo/gtk *)
(*****************************************************************************)

let width = 500
let height = 500

let test_draw cr =
  (* [0,0][1,1] world scaled to a width x height screen *)
  Cairo.scale cr (float_of_int width) (float_of_int height);

  Cairo.set_source_rgba cr ~red:0.5 ~green:0.5 ~blue:0.5 ~alpha:0.5;
  Cairo.set_line_width cr 0.001;

  Cairo.move_to cr 0.5 0.5;
  Cairo.line_to cr 0.6 0.6;
  Cairo.stroke cr;

  Cairo.select_font_face cr "monospace"
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_BOLD;
  Cairo.set_font_size cr 0.1;

  let _extent = Cairo.text_extents cr "peh" in
  (* WEIRD: if Cairo.text_extents cr "d" create an Out_of_memory exn *)
  (* related? https://github.com/diagrams/diagrams-cairo/issues/43
  *)


  Cairo.move_to cr 0.1 0.1;
  Cairo.show_text cr "THIS IS SOME TEXT";
  Cairo.move_to cr 0.1 0.2;
  Cairo.show_text cr "THIS IS SOME TEXT";
  Cairo.set_font_size cr 0.05;
  Cairo.move_to cr 0.1 0.3;
  Cairo.show_text cr "THIS IS SOME TEXT";

  Cairo.set_source_rgb cr ~red:0.1 ~green:0.1 ~blue:0.1;
  Cairo.move_to cr 0.1 0.1;
  Cairo.line_to cr 0.1 0.2;
  Cairo.stroke cr;

  let start = ref 0.0 in

  for _i = 0 to 3 do
    let end_ = !start +. 0.5 in
    Cairo.arc cr ~xc:0.5 ~yc:0.5 ~radius:0.3 ~angle1:!start
      ~angle2:end_;
    Cairo.stroke cr;
    start := end_;
  done;

  ()

let test_cairo () =
  let _locale = GtkMain.Main.init () in
  let w = GWindow.window ~title:"test" () in
  (w#connect#destroy GMain.quit) +> ignore;
  let px = GDraw.pixmap ~width ~height ~window:w () in
  px#set_foreground `WHITE;
  px#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  let cr = Cairo_lablgtk.create px#pixmap in
  test_draw cr;
  (GMisc.pixmap px ~packing:w#add ()) +> ignore;
  w#show ();
  GMain.main()


(*****************************************************************************)
(* Final view rendering *)
(*****************************************************************************)

let paint w =
  pr2 "paint";
  ()

(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

let init () =
  let _locale = GtkMain.Main.init () in

  let width = 800 in
  let height = 1010 in
  let w = {
    width;
    height;
  }
  in

  let statusbar = GMisc.statusbar () in
  let ctx = statusbar#new_context "main" in

(*
  Controller._set_title := (fun s -> win#set_title s);
  Controller._statusbar_addtext := (fun s -> ctx#push s +> ignore);
*)

  let win = GWindow.window ~title:"MMM" () in
  let quit () = GMain.Main.quit (); in


  (*-------------------------------------------------------------------*)
  (* Creation of graphic view *)
  (*-------------------------------------------------------------------*)

    let px = GDraw.pixmap ~width ~height ~window:win () in 
      px#set_foreground `WHITE;
      px#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();

      let cr = Cairo_lablgtk.create px#pixmap in
      Cairo.scale cr 1.0 1.0;
      Cairo.select_font_face cr "fixed"
        Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
      Cairo.set_font_size cr 20.0;
      
      paint w;

  (*-------------------------------------------------------------------*)
  (* Creation of core DS *)
  (*-------------------------------------------------------------------*)

  (*-------------------------------------------------------------------*)
  (* Layout *)
  (*-------------------------------------------------------------------*)

  (* if use my G.mk style for that, then get some pbs when trying
   * to draw stuff :(
   *)
  let vbox = GPack.vbox ~packing:win#add () in

    (*-------------------------------------------------------------------*)
    (* Menu *)
    (*-------------------------------------------------------------------*)
    vbox#pack (G.mk (GMenu.menu_bar) (fun m -> 

      let factory = new GMenu.factory m in

      factory#add_submenu "_File" +> (fun menu -> 
        GToolbox.build_menu menu ~entries:[
          `S;
        ];
      ) |> ignore;
      factory#add_submenu "_Help" +> (fun menu -> 
        ()
      );
    ));


    (*-------------------------------------------------------------------*)
    (* toolbar *)
    (*-------------------------------------------------------------------*)

    vbox#pack (G.mk (GButton.toolbar) (fun tb ->
      tb#insert_widget (G.mk (GMisc.label ~text:"URL: ") (fun _lbl ->
        ()
      ));
      tb#insert_widget (G.mk (GEdit.entry ~width:200) (fun entry ->

        entry#connect#changed (fun () -> 
          let s = entry#text in
          pr2 s;
        ) |> ignore;
      ));


    ));

    (*-------------------------------------------------------------------*)
    (* main view *)
    (*-------------------------------------------------------------------*)

    (GMisc.pixmap px ~packing:vbox#pack ()) |> ignore;

    (*-------------------------------------------------------------------*)
    (* status bar *)
    (*-------------------------------------------------------------------*)
    vbox#pack (*~from: `END*) statusbar#coerce;

  (*-------------------------------------------------------------------*)
  (* End *)
  (*-------------------------------------------------------------------*)

  win#connect#destroy ~callback:quit |> ignore;
  win#show ();
  GMain.main()

(*****************************************************************************)
(* main *)
(*****************************************************************************)

let _ =
  (* test_cairo () *)
  init ()
  
  

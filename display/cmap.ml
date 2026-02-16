(*s: ./display/cmap.ml *)
open Printf
open Tk
open Embed
open Maps
open Hyper


(*s: exception Cmap.Syntax *)
(* Client Side Image Maps
   We must have two modes: one when the image has not been loaded.
   In that case, we need something like a popup menu. And then, when
   the image is loaded, we use a canvas
   *)

exception Syntax of string
(*e: exception Cmap.Syntax *)

(*s: function Cmap.alt_mode *)
let alt_mode emb m l =
  Log.debug (sprintf "Alt mode map for %s" (Widget.name l));
  let menu = Menu.create_named l "map" [] in
  List.iter (fun area ->
    Menu.add_command menu
      [Label (if area.area_alt = "" then 
    area.area_link.h_uri
      else area.area_alt);
    Command (fun () -> emb.embed_context#goto area.area_link)])
    m;
  bind l (Glevents.get "alt_imap")
    (BindSet ([Ev_RootX; Ev_RootY],
          (fun ei -> Menu.popup menu ei.ev_RootX ei.ev_RootY)))
(*e: function Cmap.alt_mode *)

(*s: function Cmap.printTagOrId *)
let printTagOrId = function
  | Id n -> Log.f (sprintf "Id %d" n)
  | Tag s -> Log.f (sprintf "Tag %s" s)
(*e: function Cmap.printTagOrId *)


(* See Htbind for semantics of this class *)
class imap (c, items) =
 object (self)
 inherit Htbind.active () as super

 (* val items = items *)
 (* val c = c *)
 method widget = c

 method getlink ei =
   let cx = truncate (Canvas.canvasx c (Pixels ei.ev_MouseX))
   and cy = truncate (Canvas.canvasy c (Pixels ei.ev_MouseY)) in
     match Canvas.find c [Closest (Pixels cx, Pixels cy)] with
       [id] -> List.assoc id items
     |  _ -> raise Not_found

 method binder = Canvas.bind c (Tag "current") 

 method highlight _ = ()
 method markused _ei = ()

 method! init ctx =
   super#init ctx;
   self#binder [[], Motion]
    (BindSet ([Ev_MouseX; Ev_MouseY], 
          (fun ei ->
            try
              let link = self#getlink ei in
              ctx#invoke "pointsto" link
            with
              Not_found -> ())))
end
 



(*s: function Cmap.gfx_mode *)
(* This is called when the image has been loaded *)
let gfx_mode emb map c =
  Log.debug (sprintf "Gfx mode map for %s" (Widget.name c));

  (* Build the canvas items corresponding to active zones *)

  (* For points *inside* rects and circle items to be actually considered
     inside for the purpose of activation, we must use both an empty outline
     and an empty fill. *)
  let opts = [Outline (NamedColor ""); FillColor (NamedColor "")] in

  let items = 
    List.map (fun area ->
      try
        match area.area_kind with
      Default -> Id 1, area.area_link (* the image itself *)
        | Rect -> 
        begin match area.area_coords with
        | [x1;y1;x2;y2] ->
           Canvas.create_rectangle c
          (Pixels x1) (Pixels y1) (Pixels x2) (Pixels y2) 
          opts,
           area.area_link
        | _ -> 
        raise (Syntax "rect")
        end
        | Circle ->
        begin match area.area_coords with
        | [x;y;r] ->
           Canvas.create_oval c
          (Pixels (x-r)) (Pixels (y-r))
          (Pixels (x+r)) (Pixels (y+r))
          opts,
           area.area_link
        | _ -> raise (Syntax "circle")
        end
        | Poly ->
        let l = List.length area.area_coords in
        (* there must be at least three points, and by pair *)
        if l < 6 || l mod 2 <> 0 then begin
          Log.f "Invalid coords for polygon shape";
          raise (Syntax "polygon")
        end
        else
          Canvas.create_polygon c
               (List.map (fun x -> Pixels x) area.area_coords)
               opts,
          area.area_link
      with 
      |	Syntax s ->
      Log.f (sprintf "Wrong syntax in area mapping (%s)" s);
      Tag "area error", area.area_link
      | Protocol.TkError s -> 
      Log.f (sprintf "Error in area mapping (%s)" s);
      Tag "area error", area.area_link
     )
     map
  in
  Canvas.lower_bot c (Id 1);
  let htobj = new imap(c,items) in
    htobj#init emb.embed_context
(*e: function Cmap.gfx_mode *)


(*e: ./display/cmap.ml *)

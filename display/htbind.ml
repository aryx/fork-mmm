(*s: ./display/htbind.ml *)
(* Bindings for hypernavigation *)
open Printf
open Tk
open Hyper
open Viewers

(*
An active object is assumed to have the following methods:
*)    


class  virtual active () =
 object (self)
 method virtual widget : Widget.widget
      (* returns the widget to which an hypermenu can be attached *)
 method virtual getlink : eventInfo -> Hyper.link
      (* returns the link pointed to by the object *)
 method virtual binder : (modifier list * xEvent) list -> bindAction -> unit
      (* binds events on the object *)
 method virtual highlight : bool -> unit
      (* user feedback (mostly cursor) indicating that object is active *)
 method virtual markused : eventInfo -> unit
      (* say that we've activated this link *)

 method init (ctx : Viewers.context) =
  (* Install all navigation bindings *)
  List.iter (fun hyname ->
    try 
      let hyperf = List.assoc hyname ctx#hyper_funs in
      self#binder (Glevents.get hyname)
    (BindSet ([Ev_MouseX; Ev_MouseY],
          (fun ei -> 
             try let link = self#getlink ei in
                  self#markused ei;
                  ctx#invoke hyname link;
             with Not_found -> ())))
    with
      Not_found -> ())
    ["goto"; "save"; "gotonew"];

  (* Install the menu (created by need only) *)
  let menulink = ref None
  and menuei = ref None in
  let hypermenu = Frx_misc.autodef (fun () ->
     let m = Menu.create_named self#widget "hypermenu" [] in
        (* The first entry has its text replaced by the link url *)
        Menu.add_command m [Label ""];
        Menu.add_separator m;
    List.iter (fun (fname, f) ->
             if f.hyper_visible then
              Menu.add_command m
               [Label f.hyper_title; 
                Command (fun () -> 
                  match !menuei, !menulink with
                Some ei, Some link ->
                  self#markused ei;
                  ctx#invoke fname link
                  |	_, _ -> ())])
          ctx#hyper_funs;
    m) in
  self#binder (Glevents.get "hypermenu")
   (BindSet ([Ev_MouseX; Ev_MouseY; Ev_RootX; Ev_RootY], 
       (fun ei -> 
     try
       let link = self#getlink ei in
       menuei := Some ei; menulink := Some link;
       let m = hypermenu() in
       Menu.configure_command m (Number 1) [Label (Hyper.string_of link)];
       Menu.popup m ei.ev_RootX ei.ev_RootY
     with
       Not_found -> ())));

  (* Install the pointsto internal bindings *)
  self#binder [[], Enter]
    (BindExtend ([Ev_MouseX; Ev_MouseY], 
         (fun ei ->
           try
             let link = self#getlink ei in
             self#highlight true;
             ctx#invoke "pointsto" link
           with Not_found -> ())));
  let fakehlink =
    {h_uri = ""; h_context = None; h_method = GET; h_params = [] } in
  self#binder [[], Leave]
    (BindSet ([Ev_MouseX; Ev_MouseY], 
          (fun ei ->
        self#highlight false;
        ctx#invoke "clearpointsto" fakehlink)))
end

(*
 * The various active objects
 *)

(* Text widget with anchors marked as tags *)

class  virtual hypertext (thtml) =
 object (self)
  inherit active () as super
  (* val thtml = thtml *)  (* keep our own copy *)

  method widget = thtml

  method virtual getlink : eventInfo -> Hyper.link

  method binder = Text.tag_bind thtml "anchor" 

  method highlight flag = 
    if flag then
      Text.configure thtml [Cursor (XCursor "hand2")]
    else
      Text.configure thtml [Cursor (XCursor "xterm")]

  method virtual getrange : index -> index * index

  method markused ei =
     (* The index of the click position *)
     let i = 
       Text.index thtml (TextIndex (AtXY (ei.ev_MouseX,ei.ev_MouseY), [])) in
     (* Tags at this place *)
     let s,e = self#getrange i in
       Text.tag_add thtml "visited" (TextIndex (s,[])) (TextIndex (e,[]))

  (* we don't get Enter/Leave when tags are contiguous, so the 
     pointed link displayed in pointsto is no always correct
     Thus, extend initialisation to bind pointsto on motion
   *)
  method init ctx =
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


(* embedded objects with direct map *)
class directmap (frame, link) =
 object (self)
  inherit active ()
  (* val frame = frame *)
  method widget = frame
  val link = (link : Hyper.link)
  method getlink (ei : eventInfo) = link
  method binder = bind frame
  method highlight (flag : bool) = ()  (* we already set up the cursor *)
  method markused ei =
    Frame.configure frame [Relief Sunken]
end

(* embedded objects with server map (ISMAP) *)
(* pointsto will get some arbitrary value for x,y... *)
class servermap (frame,link) =
 object (self)
  inherit active ()
  inherit directmap (frame, link)
  method getlink ei = 
    {h_uri = sprintf "%s?%d,%d" link.h_uri
                                ei.ev_MouseX ei.ev_MouseY;
     h_context = link.h_context;
     h_method = GET;
     h_params = link.h_params}
end

(* embedded objects with form submission *)
class formmap (frame,formlink) =
 object (self)
  inherit active ()
  (* val frame = frame *)
  method widget = frame
  val formlink = (formlink : int * int -> Hyper.link)
  method getlink ei = formlink (ei.ev_MouseX, ei.ev_MouseY)
  method binder = bind frame
  method highlight (flag : bool) = ()  (* we already set up the cursor *)
  method markused ei =
    Frame.configure frame [Relief Sunken]
end


(* Client side image maps are defined in Cmap *)
(*e: ./display/htbind.ml *)

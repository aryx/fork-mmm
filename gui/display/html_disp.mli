(*s: display/html_disp.mli *)

(*s: signature [[Html_disp.verbose]] *)
val verbose : bool ref
(*e: signature [[Html_disp.verbose]] *)
(*s: signature [[Html_disp.attempt_tables]] *)
val attempt_tables : bool ref
(*e: signature [[Html_disp.attempt_tables]] *)

(*s: signature class [[Html_disp.imgloader]] *)
class  virtual imgloader : (unit) -> object
  (*s: [[Html_disp.imgloader]] virtual fields signatures *)
  method virtual add_image : Embed.obj -> unit	 (* add one image *)
  method virtual flush_images : unit	         (* flush when document is loaded *)
  method virtual load_images : unit		 (* manual flush *)
  method virtual update_images : unit
  (*e: [[Html_disp.imgloader]] virtual fields signatures *)
end
(*e: signature class [[Html_disp.imgloader]] *)

(*s: signature class [[Html_disp.machine]] *)
class  virtual machine : (unit) -> object
  (*s: [[Html_disp.machine]] virtual fields signatures *)
  (* context *)
  method virtual ctx : Viewers.context

  (* input *)
  method virtual send : Html.token -> unit
  (*s: [[Html_disp.machine]] html input other methods *)
  method virtual look_for : Html.token -> unit
  (*e: [[Html_disp.machine]] html input other methods *)

  (* semantic *)
  method virtual add_tag: 
    string -> 
    (* open handler *)  (Htmlfmt.formatter -> Html.tag -> unit) -> 
    (* close handler *) (Htmlfmt.formatter -> unit) -> 
    unit
  method virtual remove_tag : string -> unit
  (*s: [[Html_disp.machine]] tags methods *)
  method virtual get_tag : 
    string -> 
    (Htmlfmt.formatter -> Html.tag -> unit) * 
    (Htmlfmt.formatter -> unit)
  (*e: [[Html_disp.machine]] tags methods *)
  (*s: [[Html_disp.machine]] action stack methods *)
  method virtual push_action : (string -> unit) -> unit
  method virtual pop_action : unit
  (*e: [[Html_disp.machine]] action stack methods *)

  (* backend *)
  method virtual formatter : Htmlfmt.formatter
  (*s: [[Html_disp.machine]] formatter stack methods *)
  method virtual push_formatter : Htmlfmt.formatter -> unit
  method virtual pop_formatter : Htmlfmt.formatter
  (*e: [[Html_disp.machine]] formatter stack methods *)
  (*s: [[Html_disp.machine]] formatter misc methods *)
  method virtual create_formatter : 
    Htmlfmt.formatterSpec -> Widget.widget -> Htmlfmt.formatter * Widget.widget
  (*e: [[Html_disp.machine]] formatter misc methods *)

  (* special tags *)
  (*s: [[Html_disp.machine]] embedded fields *)
  method virtual add_embedded : Embed.obj -> unit
  method virtual embedded : Embed.obj list
  (*e: [[Html_disp.machine]] embedded fields *)
  (*s: [[Html_disp.machine]] image methods *)
  method virtual imgmanager : imgloader
  (*e: [[Html_disp.machine]] image methods *)
  (*s: [[Html_disp.machine]] fragment method *)
  method virtual see_frag : string option -> unit
  (*e: [[Html_disp.machine]] fragment method *)

  (* misc *)
  (*s: [[Html_disp.machine]] i18 methods *)
  (* For other languages) *)
  (* encode the internal i18n strings to corresponding encodings *)
  method virtual i18n_encoder : string -> string
  method virtual set_i18n_encoder : (string -> string) -> unit
  (*e: [[Html_disp.machine]] i18 methods *)
  (*s: [[Html_disp.machine]] other fields *)
  method virtual base : string
  method virtual set_base : string -> unit
  (*x: [[Html_disp.machine]] other fields *)
  method virtual target : string option
  method virtual set_target : string -> unit
  (*e: [[Html_disp.machine]] other fields *)
  (*e: [[Html_disp.machine]] virtual fields signatures *)
end
(*e: signature class [[Html_disp.machine]] *)

(*s: signature [[Html_disp.add_hook]] *)
val add_hook: (machine -> unit) -> unit
(*e: signature [[Html_disp.add_hook]] *)

(*s: signature functor [[Html_disp.Make]] *)
(*module Make 
  (G: Htmlfmt.GfxHTML) 
  (F: Htmlfmt.FormDisplay) 
  (T: Htmlfmt.TableDisplay) 
  : sig
    (* Do we need to export FormLogic and TableLogic so that extensions
     *  can access them ?
     *)
*)
val create : Viewers.context * imgloader -> machine
(*end*)
(*e: signature functor [[Html_disp.Make]] *)
(*e: display/html_disp.mli *)

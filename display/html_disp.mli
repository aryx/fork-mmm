(*s: ./display/html_disp.mli *)

(*s: signature Html_disp.verbose *)
val verbose : bool ref
(*e: signature Html_disp.verbose *)
(*s: signature Html_disp.attempt_tables *)
val attempt_tables : bool ref
(*e: signature Html_disp.attempt_tables *)

(*s: signature class Html_disp.imgloader *)
class  virtual imgloader : (unit) -> object
  (*s: [[Html_disp.imgloader]] virtual fields signatures *)
  method virtual add_image : Embed.embobject -> unit	 (* add one image *)
  method virtual flush_images : unit	         (* flush when document is loaded *)
  method virtual load_images : unit		 (* manual flush *)
  method virtual update_images : unit
  (*e: [[Html_disp.imgloader]] virtual fields signatures *)
end
(*e: signature class Html_disp.imgloader *)

(*s: signature class Html_disp.machine *)
class  virtual machine : (unit) -> object
  (*s: [[Html_disp.machine]] virtual fields signatures *)
  method virtual ctx : Viewers.context
  (*x: [[Html_disp.machine]] virtual fields signatures *)
  method virtual base : string
  method virtual set_base : string -> unit
  (*x: [[Html_disp.machine]] virtual fields signatures *)
  method virtual target : string option
  method virtual set_target : string -> unit
  (*x: [[Html_disp.machine]] virtual fields signatures *)
  method virtual add_tag: 
    string -> 
    (* open handler *)  (Htmlfmt.formatter -> Html.tag -> unit) -> 
    (* close handler *) (Htmlfmt.formatter -> unit) -> 
    unit
  method virtual get_tag : 
    string -> 
    (Htmlfmt.formatter -> Html.tag -> unit) * 
    (Htmlfmt.formatter -> unit)
  method virtual remove_tag : string -> unit
  (*x: [[Html_disp.machine]] virtual fields signatures *)
  method virtual push_action : (string -> unit) -> unit
  method virtual pop_action : unit
  (*x: [[Html_disp.machine]] virtual fields signatures *)
  method virtual formatter : Htmlfmt.formatter
  (*x: [[Html_disp.machine]] virtual fields signatures *)
  method virtual push_formatter : Htmlfmt.formatter -> unit
  method virtual pop_formatter : Htmlfmt.formatter
  (*x: [[Html_disp.machine]] virtual fields signatures *)
  method virtual create_formatter : 
    Htmlfmt.formatterSpec -> Widget.widget -> Htmlfmt.formatter * Widget.widget
  (*x: [[Html_disp.machine]] virtual fields signatures *)
  method virtual look_for : Html.token -> unit
  method virtual send : Html.token -> unit
  (*x: [[Html_disp.machine]] virtual fields signatures *)
  method virtual add_embedded : Embed.embobject -> unit
  method virtual embedded : Embed.embobject list
  (*x: [[Html_disp.machine]] virtual fields signatures *)
  method virtual imgmanager : imgloader
  (*x: [[Html_disp.machine]] virtual fields signatures *)
  method virtual see_frag : string option -> unit
  (*x: [[Html_disp.machine]] virtual fields signatures *)
  (* For other languages) *)
  (* encode the internal i18n strings to corresponding encodings *)
  method virtual i18n_encoder : string -> string
  method virtual set_i18n_encoder : (string -> string) -> unit
  (*e: [[Html_disp.machine]] virtual fields signatures *)
end
(*e: signature class Html_disp.machine *)

(*s: signature Html_disp.add_hook *)
val add_hook: (machine -> unit) -> unit
(*e: signature Html_disp.add_hook *)

(*s: signature functor Html_disp.Make *)
module Make 
  (G: Htmlfmt.GfxHTML) 
  (F: Htmlfmt.FormDisplay) 
  (T: Htmlfmt.TableDisplay) 
  : sig
    (* Do we need to export FormLogic and TableLogic so that extensions
     *  can access them ?
     *)
    val create : Viewers.context * imgloader -> machine
end
(*e: signature functor Html_disp.Make *)
(*e: ./display/html_disp.mli *)

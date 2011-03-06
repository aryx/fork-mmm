open Htmlfmt
open Embed

val attempt_tables : bool ref
val verbose : bool ref

class  virtual imgloader : (unit) -> object
  method virtual add_image : embobject -> unit	 (* add one image *)
  method virtual flush_images : unit	         (* flush when document is loaded *)
  method virtual load_images : unit		 (* manual flush *)
  method virtual update_images : unit
end


class  virtual machine : (unit) -> object
  method virtual formatter : formatter
  method virtual imgmanager : imgloader
  method virtual base : string
  method virtual set_base : string -> unit
  method virtual target : string option
  method virtual set_target : string -> unit
  method virtual ctx : Viewers.context
  method virtual add_tag : string -> (formatter -> Html.tag -> unit) -> (formatter -> unit) -> unit
  method virtual get_tag : string -> (formatter -> Html.tag -> unit) * (formatter -> unit)
  method virtual remove_tag : string -> unit
  method virtual push_action : (string -> unit) -> unit
  method virtual pop_action : unit
  method virtual push_formatter : formatter -> unit
  method virtual pop_formatter : formatter
  method virtual create_formatter : formatterSpec -> Widget.widget -> formatter * Widget.widget
  method virtual send : Html.token -> unit
  method virtual look_for : Html.token -> unit
  method virtual add_embedded : embobject -> unit
  method virtual embedded : embobject list
  method virtual see_frag : string option -> unit
    
  (* For Japanese (or other langs) *)
  (* encode the internal i18n strings to corresponding encodings *)
  method virtual i18n_encoder : string -> string
  method virtual set_i18n_encoder : (string -> string) -> unit
end


val add_hook: (machine -> unit) -> unit

module Make (G : GfxHTML) (F: FormDisplay) (T: TableDisplay) : sig

(* Do we need to export FormLogic and TableLogic so that extensions
   can access them ?
 *)

val create : Viewers.context * imgloader -> machine

end

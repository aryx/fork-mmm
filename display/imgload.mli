
type mode =
  | DuringDoc
  | AfterDocAuto
  | AfterDocManual

val mode : mode ref
val no_images : bool ref
val gif_anim_auto : bool ref

class loader : (unit) -> object
  method add_image : Embed.embobject -> unit	 (* add one image *)
  method flush_images : unit	         (* flush when document is loaded *)
  method load_images : unit		 (* manual flush *)
  method update_images : unit
end

val create : unit -> loader


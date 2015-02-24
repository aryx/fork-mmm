(*s: ./display/imgload.mli *)

(*s: enum Imgload.mode *)
type mode =
  | DuringDoc
  | AfterDocAuto
  | AfterDocManual
(*e: enum Imgload.mode *)

(*s: signature Imgload.mode *)
val mode : mode ref
(*e: signature Imgload.mode *)
(*s: signature Imgload.no_images *)
val no_images : bool ref
(*e: signature Imgload.no_images *)
(*s: signature Imgload.gif_anim_auto *)
val gif_anim_auto : bool ref
(*e: signature Imgload.gif_anim_auto *)

class loader : (unit) -> object
  method add_image : Embed.embobject -> unit	 (* add one image *)
  method flush_images : unit	         (* flush when document is loaded *)
  method load_images : unit		 (* manual flush *)
  method update_images : unit
end

(*s: signature Imgload.create *)
val create : unit -> loader
(*e: signature Imgload.create *)

(*e: ./display/imgload.mli *)

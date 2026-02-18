(*s: display/imgload.mli *)

(*s: type [[Imgload.mode]] *)
type mode =
  | DuringDoc
  | AfterDocAuto
  | AfterDocManual
(*e: type [[Imgload.mode]] *)

(*s: signature [[Imgload.mode]] *)
val mode : mode ref
(*e: signature [[Imgload.mode]] *)
(*s: signature [[Imgload.no_images]] *)
val no_images : bool ref
(*e: signature [[Imgload.no_images]] *)
(*s: signature [[Imgload.gif_anim_auto]] *)
val gif_anim_auto : bool ref
(*e: signature [[Imgload.gif_anim_auto]] *)

(*s: class [[Imgload.loader]] signature *)
class loader : (unit) -> object
  method add_image : Embed.obj -> unit       (* add one image *)
  method flush_images : unit	         (* flush when document is loaded *)
  method load_images : unit		(* manual flush *)
  method update_images : unit
end
(*e: class [[Imgload.loader]] signature *)

(*s: signature [[Imgload.create]] *)
val create : unit -> loader
(*e: signature [[Imgload.create]] *)

(*e: display/imgload.mli *)

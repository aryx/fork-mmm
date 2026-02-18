(*s: retrieve/img.mli *)
open Document
(*s: signature [[Img.gif_anim_load]] *)
val gif_anim_load : bool ref
(*e: signature [[Img.gif_anim_load]] *)

module ImageData : sig
  type t = Tkanim.imageType

  val gamma : float ref
  val jpeg_converter : string ref
  val verbose : bool ref

  val load : handle -> Document.id list -> Fpath.t -> Tkanim.imageType
  val cache_access : Url.t -> Document.id -> Tkanim.imageType
  val error :
      Url.t -> (Document.id * ((Url.t -> Tkanim.imageType -> unit) * Scheduler.progress_func)) list -> unit
    val error_msg : Www.request * string -> unit
    val remove_reference : Document.id -> unit
    val dump: unit -> unit
  end

module ImageScheduler : Scheduler.S with
    type shared_data = ImageData.t

(*s: signature [[Img.get]] *)
val get : Document.id -> Hyper.link -> (Url.t -> ImageData.t -> unit) -> 
            Scheduler.progress_func -> unit
(*e: signature [[Img.get]] *)
(*s: signature [[Img.update]] *)
val update : Url.t -> unit
(*e: signature [[Img.update]] *)
(*e: retrieve/img.mli *)

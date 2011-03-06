open Document
val gif_anim_load : bool ref

module ImageData : sig
  type t = Tkanim.imageType
  val gamma : float ref
  val jpeg_converter : string ref
  val verbose : bool ref

  val load : handle -> document_id list -> string -> Tkanim.imageType
  val cache_access : Url.t -> document_id -> Tkanim.imageType
  val error :
      Url.t -> (document_id * ((Url.t -> Tkanim.imageType -> unit) * Scheduler.progress_func)) list -> unit
    val error_msg : Www.request * string -> unit
    val remove_reference : document_id -> unit
    val dump: unit -> unit
  end

module ImageScheduler : Scheduler.S with
    type shared_data = ImageData.t

val get : document_id -> Hyper.link -> (Url.t -> ImageData.t -> unit) -> 
            Scheduler.progress_func -> unit
val update : Url.t -> unit

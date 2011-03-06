(*
 * Certain kind of documents need to be shared, such as in-lined images.
 * In this case, instead of working with Retrieve.f and the normal
 * document continuation, we queue the request to a scheduler, with a
 * continuation to be applied to an object representing the shared 
 * information for that document.
 * E.G: for in-lined images, the shared information is the Tk-handle to
 * the image.
 *)

val debug : bool ref

type progress_func = int option -> int -> unit

module type Data =
  sig
   type t
        (* Type of shared objects
         * The table of objects in managed in this module
         *)
   val load : 
     Document.handle -> Document.document_id list -> string -> t
        (* [load dh referers file]
	 *   is responsible for creating the shared handle
         *)
   val cache_access : Url.t -> Document.document_id -> t
        (* [cache_access url referer]
         *   attempts to find a shared handle for an URL.
         *   Raises Not_found
         *)       	     
   val error : 
        Url.t -> 
	  (Document.document_id * ((Url.t -> t -> unit) * progress_func)) list -> unit
        (* [error url [(did,(cont,progress))]]
         *  if an error occurs, then each pending continuation is called
         *  (if necessary) as required (e.g. with "default" information)
         *)
   val error_msg : Www.request * string -> unit
       (* Retrieval produces Invalid_url *)
  end
   

module type S =
  sig
    type shared_data
    val add_request : Www.request -> Document.document_id ->
      	       	       (Url.t -> shared_data -> unit) -> progress_func -> unit
        (* [add_request delayed wr referer cont progress_func]
         *   returns job handle that can subsequently by awakened
         *)

    val stop : Document.document_id -> unit
        (* [stop did]
         *   stops jobs for which did is the only referer
         *)

    (* Delayed queues for this scheduler *)
    type delayed
    val new_delayed : unit -> delayed
    val add_delayed : 
      	delayed -> Www.request -> Document.document_id -> 
            (Url.t -> shared_data -> unit) -> progress_func -> unit
    val flush_delayed : delayed -> unit
    val flush_one : delayed -> Url.t -> unit
    val is_empty : delayed -> bool
    val maxactive : int ref
    val maxsamehost : int ref
  end


module Make(J : Data):(S with type shared_data = J.t)

module Tkanim : sig
(*** Data types ***)

type animatedGif

    (* This data type contains all the information of an animation of
       gif89a format. It is still test implementation, so I should 
       keep it abstract. --- JPF *)

type imageType =
    Still of Tk.options
  | Animated of animatedGif

      (* This data type is required to distinguish normal still images
         and animated gifs. Usually objects typed imagePhoto or
	 imageBitmap are used for Still. *)

(*** Library availability check ***)

val available : unit -> bool

      (* [available ()] returns true if there is Tkanim Tcl/Tk
	 extension linked statically/dynamically in Tcl/Tk
	 interpreter. Otherwise, return false. *)

(*** User intaface ***)

(* create reads file... So, it is not safe. *)
(* val create  : string -> imageType *)

val delete  : animatedGif -> unit

      (* [delete anim] deletes all the images in anim. Usually
         animatedGifs contain many images, so you must not forget to
	 use this function to free the memory. *)

val width   : animatedGif -> int
val height  : animatedGif -> int

      (* [width anim] and [height anim] return the width and height of
	 given animated gif. *)

val images  : animatedGif -> Tk.imagePhoto list

      (* [images anim] returns the list of still images used in the 
	 animation *)

val animate : Widget.widget -> animatedGif -> bool -> unit
val animate_canvas_item : Widget.widget -> Tk.tagOrId -> animatedGif -> bool -> unit

      (*   They are the display function for animated gifs. Because
         animatedGif is abstracted, they are the only way to do it. 
	 [animate label anim] and [animate_canvas_item canvas tag anim]
	 display animations [anim] on a label widget [label] and an
	 image tag [tag] on a canvas widget [canvas] respectively.
	 Note that animation is stopped as default.
           These functions return interface functions, say, 
	 [inter : bool -> unit]. Currently, [inter false] toggles 
	 start/stop of the animation, and [inter true] displays the
	 next frame of the animation if it is stopped. *)

val gifdata : string -> imageType

      (* [gifdata data] reads [data] as a row data of a gif file and
	 decode it. You can use this funciton to embed images in your
	 applets. *)

  end


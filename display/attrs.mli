(*s: ./display/attrs.mli *)
module TagSet : Set.S with type elt = string

class tags :
  Widget.widget ->
  object
    val mutable configured : TagSet.t
    val mutable decorations : (Tk.textTag * Tk.textIndex * Tk.textIndex) list
    val mutable onhold : (TagSet.elt * Tk.options list) list
    val wid : Widget.widget
    method add : Tk.textTag * Tk.textIndex * Tk.textIndex -> unit
    method change : TagSet.elt -> Tk.options list -> unit
    method define : TagSet.elt -> Tk.options list -> unit
    method flush : unit
  end

module LocMap :
  sig
    type key = Tk.index
    type 'a t
    val empty : 'a t
    val add : key * key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val find_interval : key -> 'a t -> key * key
  end

class anchortags :
  Widget.widget ->
  object
    val mutable anchor_table : Hyper.link LocMap.t
    val mutable configured : TagSet.t
    val mutable decorations : (Tk.textTag * Tk.textIndex * Tk.textIndex) list
    val mutable mappings : (Tk.textIndex * Tk.textIndex * Hyper.link) list
    val mutable onhold : (TagSet.elt * Tk.options list) list
    val wid : Widget.widget
    method add : Tk.textTag * Tk.textIndex * Tk.textIndex -> unit
    method add_anchor : Tk.textIndex -> Tk.textIndex -> Hyper.link -> unit
    method binder :
      (Tk.modifier list * Tk.xEvent) list -> Tk.bindAction -> unit
    method change : TagSet.elt -> Tk.options list -> unit
    method define : TagSet.elt -> Tk.options list -> unit
    method flush : unit
    method getlink : Tk.eventInfo -> Hyper.link
    method getrange : LocMap.key -> LocMap.key * LocMap.key
    method highlight : bool -> unit
    method init : Viewers.context -> unit
    method markused : Tk.eventInfo -> unit
    method widget : Widget.widget
  end

class virtual ['a] nested :
  < add : string * Tk.textIndex * Tk.textIndex -> 'b;
    define : string -> Tk.options list -> 'c; .. > ->
  object
    val mutable last_change : Tk.textIndex
    val mutable stack : string list
    method pop : Tk.textIndex -> 'a -> unit
    method virtual pop_convert : 'a -> unit
    method push : Tk.textIndex -> 'a -> unit
    method virtual push_convert : 'a -> string * Tk.options list
    method put : Tk.textIndex -> string -> unit
  end

class align :
  < add : string * Tk.textIndex * Tk.textIndex -> 'a;
    define : string -> Tk.options list -> 'b; .. > ->
  object
    val mutable last_change : Tk.textIndex
    val mutable stack : string list
    method pop : Tk.textIndex -> string -> unit
    method pop_convert : string -> unit
    method push : Tk.textIndex -> string -> unit
    method push_convert : string -> string * Tk.options list
    method put : Tk.textIndex -> string -> unit
  end

class margin :
  < add : string * Tk.textIndex * Tk.textIndex -> 'a;
    define : string -> Tk.options list -> 'b; .. > ->
  object
    val mutable current : int
    val mutable last_change : Tk.textIndex
    val mutable stack : string list
    method pop : Tk.textIndex -> int -> unit
    method pop_convert : int -> unit
    method push : Tk.textIndex -> int -> unit
    method push_convert : int -> string * Tk.options list
    method put : Tk.textIndex -> string -> unit
  end

class font :
  < add : string * Tk.textIndex * Tk.textIndex -> 'a;
    define : string -> Tk.options list -> 'b; .. > ->
  object
    val mutable basefont : Fonts.fontDesc
    val mutable font_stack : Fonts.fontDesc list
    val mutable last_change : Tk.textIndex
    val mutable stack : string list
    method pop : Tk.textIndex -> Fonts.fontAttrs -> unit
    method pop_all : Tk.textIndex -> unit
    method pop_convert : Fonts.fontAttrs -> unit
    method push : Tk.textIndex -> Fonts.fontAttrs -> unit
    method push_convert : Fonts.fontAttrs -> string * Tk.options list
    method put : Tk.textIndex -> string -> unit
    method set_base : Tk.textIndex -> int -> unit
  end

(*s: signature Attrs.color_mappings *)
val color_mappings : (string, string) Hashtbl.t
(*e: signature Attrs.color_mappings *)
(*s: signature Attrs.html_color *)
val html_color : string -> string
(*e: signature Attrs.html_color *)

class fgcolor :
  < add : string * Tk.textIndex * Tk.textIndex -> 'a;
    define : string -> Tk.options list -> 'b; .. > ->
  object
    val mutable last_change : Tk.textIndex
    val mutable stack : string list
    method pop : Tk.textIndex -> string -> unit
    method pop_convert : string -> unit
    method push : Tk.textIndex -> string -> unit
    method push_convert : string -> string * Tk.options list
    method put : Tk.textIndex -> string -> unit
  end

class bgcolor :
  < add : string * Tk.textIndex * Tk.textIndex -> 'a;
    define : string -> Tk.options list -> 'b; .. > ->
  object
    val mutable last_change : Tk.textIndex
    val mutable stack : string list
    method pop : Tk.textIndex -> string -> unit
    method pop_convert : string -> unit
    method push : Tk.textIndex -> string -> unit
    method push_convert : string -> string * Tk.options list
    method put : Tk.textIndex -> string -> unit
  end

class offset :
  < add : string * Tk.textIndex * Tk.textIndex -> 'a;
    define : string -> Tk.options list -> 'b; .. > ->
  object
    val mutable cur_offset : int
    val mutable last_change : Tk.textIndex
    val mutable stack : string list
    method pop : Tk.textIndex -> int -> unit
    method pop_convert : int -> unit
    method push : Tk.textIndex -> int -> unit
    method push_convert : int -> string * Tk.options list
    method put : Tk.textIndex -> string -> unit
  end

class misc :
  < add : 'a * Tk.textIndex * Tk.textIndex -> unit; define : 'a -> 'b -> 'c;
    .. > *
  'a * 'b ->
  object
    val mutable start_pos : Tk.textIndex
    val tagname : 'a
    method pop : Tk.textIndex -> unit
    method push : Tk.textIndex -> unit
  end

class spacing :
  < add : string * Tk.textIndex * Tk.textIndex -> 'a;
    define : string -> Tk.options list -> 'b; .. > ->
  object
    method pop : Tk.textIndex -> int -> unit
    method push : Tk.textIndex -> int -> unit
  end

(*s: signature Attrs.circle_data *)
val circle_data : string
(*e: signature Attrs.circle_data *)
(*s: signature Attrs.disc_data *)
val disc_data : string
(*e: signature Attrs.disc_data *)
(*s: signature Attrs.square_data *)
val square_data : string
(*e: signature Attrs.square_data *)
(*s: signature Attrs.bullet_table *)
val bullet_table : (string, Tk.options) Hashtbl.t
(*e: signature Attrs.bullet_table *)
(*s: signature Attrs.init *)
val init : string -> unit
(*e: signature Attrs.init *)
(*e: ./display/attrs.mli *)


class virtual active :
  unit ->
  object
    method virtual binder :
      (Tk.modifier list * Tk.xEvent) list -> Tk.bindAction -> unit
    method virtual getlink : Tk.eventInfo -> Hyper.link
    method virtual highlight : bool -> unit
    method init : Viewers.context -> unit
    method virtual markused : Tk.eventInfo -> unit
    method virtual widget : Widget.widget
  end

class virtual hypertext :
  Widget.widget ->
  object
    method binder :
      (Tk.modifier list * Tk.xEvent) list -> Tk.bindAction -> unit
    method virtual getlink : Tk.eventInfo -> Hyper.link
    method virtual getrange : Tk.index -> Tk.index * Tk.index
    method highlight : bool -> unit
    method init : Viewers.context -> unit
    method markused : Tk.eventInfo -> unit
    method widget : Widget.widget
  end

class directmap :
  Widget.widget * Hyper.link ->
  object
    val link : Hyper.link
    method binder :
      (Tk.modifier list * Tk.xEvent) list -> Tk.bindAction -> unit
    method getlink : Tk.eventInfo -> Hyper.link
    method highlight : bool -> unit
    method init : Viewers.context -> unit
    method markused : Tk.eventInfo -> unit
    method widget : Widget.widget
  end

class servermap :
  Widget.widget * Hyper.link ->
  object
    val link : Hyper.link
    method binder :
      (Tk.modifier list * Tk.xEvent) list -> Tk.bindAction -> unit
    method getlink : Tk.eventInfo -> Hyper.link
    method highlight : bool -> unit
    method init : Viewers.context -> unit
    method markused : Tk.eventInfo -> unit
    method widget : Widget.widget
  end

class formmap :
  Widget.widget * (int * int -> Hyper.link) ->
  object
    val formlink : int * int -> Hyper.link
    method binder :
      (Tk.modifier list * Tk.xEvent) list -> Tk.bindAction -> unit
    method getlink : Tk.eventInfo -> Hyper.link
    method highlight : bool -> unit
    method init : Viewers.context -> unit
    method markused : Tk.eventInfo -> unit
    method widget : Widget.widget
  end

(*s: ./display/html_form.mli *)

class behaviour :
  string * Html.tag * string option * (string -> string) ->
  object
    val action : string
    val mutable elem_reset : (unit -> unit) list
    val mutable elem_value :
      (Htmlfmt.input_kind * (unit -> (string * string) list)) list
    val encoding : string
    val mutable entries : int
    val fmethod : string
    val h_params : (string * string) list
    method add_get :
      Htmlfmt.input_kind -> (unit -> (string * string) list) -> unit
    method add_reset : (unit -> unit) -> unit
    method reset : unit
    method single_submit : Hyper.link option
    method submit : (string * string) list -> Hyper.link
  end

module Make :
  functor (FormDisplay : Htmlfmt.FormDisplay) ->
    sig
      val init :
        < add_tag : string ->
                    (Htmlfmt.formatter -> Html.tag -> unit) ->
                    (Htmlfmt.formatter -> unit) -> unit;
          base : string; ctx : Viewers.context;
          i18n_encoder : string -> string;
          imgmanager : < add_image : Embed.embobject -> unit; .. >;
          pop_action : unit; push_action : (string -> unit) -> unit;
          remove_tag : string -> unit; target : string option; .. > ->
        unit
    end
(*e: ./display/html_form.mli *)

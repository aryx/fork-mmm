(*s: ./display/html_table.mli *)
(*
module Make :
  functor (TableDisplay : Htmlfmt.TableDisplay) ->
    sig
      val init :
        < add_tag : string ->
                    (Htmlfmt.formatter -> Html.tag -> unit) ->
                    (Htmlfmt.formatter -> unit) -> unit;
          create_formatter : Htmlfmt.formatterSpec ->
                             Widget.widget -> 'a * Widget.widget;
          pop_formatter : 'b; push_formatter : 'a -> 'c;
          remove_tag : string -> unit; .. > ->
        unit
    end
*)
val init: 
        < add_tag : string ->
                    (Htmlfmt.formatter -> Html.tag -> unit) ->
                    (Htmlfmt.formatter -> unit) -> unit;
          create_formatter : Htmlfmt.formatterSpec ->
                             Widget.widget -> 'a * Widget.widget;
          pop_formatter : 'b; push_formatter : 'a -> 'c;
          remove_tag : string -> unit; .. > ->
        unit

(*e: ./display/html_table.mli *)

(* TABLES *)

val debug : bool ref
val strict_32 : bool ref

type cell_type = HeaderCell | DataCell

type t = {
  table_master : Widget.widget;
  add_col : Html.tag -> unit;
  open_row : Html.tag -> unit;
  close_row : unit -> unit;
  close_table : unit -> unit;
  new_cell : cell_type -> Html.tag -> Widget.widget -> string -> Htmlfmt.width_constraint;
  bound : unit -> bool
  }

val create : Widget.widget -> Html.tag -> Htmlfmt.width_constraint -> t

val topwidth : Widget.widget -> int

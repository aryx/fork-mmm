(*s: ./display/table.mli *)
(*s: signature Table.debug *)
(* TABLES *)

val debug : bool ref
(*e: signature Table.debug *)
(*s: signature Table.strict_32 *)
val strict_32 : bool ref
(*e: signature Table.strict_32 *)

(*s: type Table.cell_type *)
type cell_type = HeaderCell | DataCell
(*e: type Table.cell_type *)

(*s: type Table.t *)
type t = {
  table_master : Widget.widget;
  add_col : Html.tag -> unit;
  open_row : Html.tag -> unit;
  close_row : unit -> unit;
  close_table : unit -> unit;
  new_cell : cell_type -> Html.tag -> Widget.widget -> string -> Htmlfmt.width_constraint;
  bound : unit -> bool
  }
(*e: type Table.t *)

(*s: signature Table.create *)
val create : Widget.widget -> Html.tag -> Htmlfmt.width_constraint -> t
(*e: signature Table.create *)

(*s: signature Table.topwidth *)
val topwidth : Widget.widget -> int
(*e: signature Table.topwidth *)
(*e: ./display/table.mli *)

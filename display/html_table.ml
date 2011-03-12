open Html
open Htmlfmt

(* 
HTML Tables are defined by RFC1942, e.g.
  <URL:ftp://ds.internic.net/rfc/rfc1942.txt>

This code *assumes* that minimisation rules are used for
cells (td and th) and for rows.
 *)


module Make (TableDisplay: TableDisplay) =
struct
open TableDisplay

let init mach =

  (* Tables may be nested, so we need to remember *)
  let table_stack = ref ([] : TableDisplay.t list) in

  (* Access to the stack *)
  let tm () = match !table_stack with
      tm::_ -> tm
    | [] -> raise (Invalid_Html "Table element outside <TABLE></TABLE>")
  and pop_table () = match !table_stack with
    | tm::l -> table_stack := l
    | [] -> raise (Invalid_Html "Unmatched </TABLE>")
  and push_table tm = table_stack := tm :: !table_stack
  and is_nested () = 
    match !table_stack with
      [] -> false
    | _ -> true
  in

  (* Layout information : the current constraint width *)

  let widths = ref [] in (* because tables are nested *)
  let current_width () = 
    match !widths with
      [] -> TopWidth
    | x::l -> x
  and push_width w =
    widths := w :: !widths
  and pop_width () = 
    match !widths with
      [] -> ()
    | x::l -> widths := l
  in
  (* <TABLE> starts a table *)
  let open_table fo t =
    fo.new_paragraph();
    (* Create the widget for embedding this table *)
    let fr = fo.create_embedded "" None None in
    (* And the table manager *)
    let tm = TableDisplay.create fr t (current_width()) in 
    (* push the table on the stack *)
    push_table tm;
    (* define behavior of other tags *)
    (* align/valign attributes *)
    let current_row_align = ref None
    and current_row_valign = ref None
    in
    let change_aligns t =
      current_row_align := 
	 (try Some (String.lowercase (get_attribute t "align"))
	  with Not_found -> None);
      current_row_valign := 
	 (try Some (String.lowercase (get_attribute t "valign"))
	  with Not_found -> None)
    in 

   let cell_aligns attrs =
     begin try Some (String.lowercase (get_attribute attrs "align"))
	   with Not_found -> !current_row_align
     end,
     begin try Some (String.lowercase (get_attribute attrs "valign"))
	   with Not_found -> !current_row_valign
     end
   in
    (* <TR> : starts a row *)
    let open_tr fo t = change_aligns t; tm.open_row t
    and close_tr fo = tm.close_row() in
    mach#add_tag "tr" open_tr close_tr;

    (* A new cell *)
    let open_cell kind fo t =
      let align,valign = cell_aligns t in
      let align = match align with 
	Some align -> align
      |	None -> match kind with 
	  HeaderCell -> "center"
	| DataCell -> "left"
      in
      (* Create a new formatter, given as parent the table widget *)
      let formatter, tcell = 
	mach#create_formatter NestedFormatter tm.table_master in
	mach#push_formatter formatter;
	push_width (tm.new_cell kind t tcell align)

    and close_cell fo =
      (* fo is the formatter that was open for *this* cell *)
      fo.flush();
      (* pop it *)
      mach#pop_formatter;
      pop_width()
    in
    mach#add_tag "th" (open_cell HeaderCell) close_cell;
    mach#add_tag "td" (open_cell DataCell) close_cell;
    mach#add_tag "col" (fun fo t -> tm.add_col t) (fun _ -> ());

  and close_table fo = 
    (* close the table manager *)
    (tm()).close_table();
    pop_table();
    fo.close_paragraph ();
     (* NOTE: this is the correct fo only if minimisation were applied
        and the correct current formatter is passed to close table
      *)
    (* remove tags *)
    List.iter mach#remove_tag ["tr";"th";"td";"col"];
  in

  mach#add_tag "table" open_table close_table;

end

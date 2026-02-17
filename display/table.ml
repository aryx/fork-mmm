(*s: display/table.ml *)
open Printf
open Tk
open Html
open Htmlfmt

(*s: constant [[Table.debug]] *)
(* Table support using the grid manager and a gross hack to obtain 
   resizing of a text widget to show its entire content.

 * Notes:
  1  we must keep geometry propagation on the grid, otherwise we'll never
     get vertical resizing
  2  the same is valid for each cell (frame around text)
  3  the spec says that the table should grow to fit its contents. However,
     this is ambiguous because in practice we must limit the width to the
     currently displayed page width.
     Having geometry propagation turned on, and letting all cells grow will
     of course keep the grid growing...
     Thus we have to set a maximum width for each cell.
     For text cells, we have to put a limit on their "automatic" horizontal
     resizing. When the limit is reached, we switch to vertical resizing,
     resetting wordwrap if allowed.
 *)

let debug = ref false
(*e: constant [[Table.debug]] *)
(*s: constant [[Table.strict_32]] *)
let strict_32 = ref true
    (* in this mode, we ignore WIDTH of TD defined with %
       This is also better for pages written for MSIE where you find
       either TD WIDTH=100% or TD WIDTH=NN
       *)
(*e: constant [[Table.strict_32]] *)

(*s: type [[Table.cell_type]] ([[display/table.ml]]) *)
(* a manager for a single TABLE *)
type cell_type = HeaderCell | DataCell
(*e: type [[Table.cell_type]] ([[display/table.ml]]) *)
(*s: type [[Table.t]] ([[display/table.ml]]) *)
type t = {
  table_master : Widget.widget;
  add_col : Html.tag -> unit;
  open_row : Html.tag -> unit;
  close_row : unit -> unit;
  close_table : unit -> unit;
  new_cell : cell_type -> Html.tag -> Widget.widget -> string -> width_constraint;
  bound : unit -> bool
  }
(*e: type [[Table.t]] ([[display/table.ml]]) *)

(*s: type [[Table.table]] *)
(* Internal structure of tables *)
type table = {
  master_widget : Widget.widget;
  width : length;
  mutable slaves :
      (Widget.widget * (int*int*int*int*width_constraint*length*string)) list;
  mutable cur_col : int;
  mutable cur_row : int;
  mutable slots : int array;
  mutable cols : int option list;
  cellpadding : int;
  cellspacing : int
  }
(*e: type [[Table.table]] *)

(*s: function [[Table.topwidth]] *)
(* Get up to the widget that has HFrame class, or to toplevel *)
let topwidth wid =
  let f = ref wid in
  try
    while true do
      let cl = Winfo.class_name !f in
      if List.mem cl ["MMM"; "HFrame"] then raise Exit
      else f := Winfo.parent !f
    done;
    0
  with
    Exit ->    
      truncate (float (Winfo.width !f) *. 0.95)
(*e: function [[Table.topwidth]] *)


(*s: function [[Table.text_align]] *)
let text_align cell align =
  Text.tag_add cell "align" Frx_text.textBegin Frx_text.textEnd;
  Text.tag_configure cell "align"
    (match align with
      "right" -> [Justify Justify_Right]
    | "center" -> [Justify Justify_Center]
    | _ -> [Justify Justify_Left])
(*e: function [[Table.text_align]] *)

(*s: function [[Table.dynamic_fight]] *)
(* Fight for your life ! *)
let dynamic_fight cell nowrap gameover align =
  match Winfo.class_name cell with
  | "Text" ->
      if !debug then
    Log.f (sprintf "DYNAMIC %s" (Widget.name cell));
      let when_finished () =
    if !debug then
      Log.f (sprintf "Switching %s to vertical resize"
                     (Widget.name cell));
        (* in all cases, we have to grow vertically *)
       let scroll, check = Fit.vert cell in
       Text.configure cell [YScrollCommand scroll];
        (* A posteriori updates for embedded windows
       List.iter 
      (fun embedded ->
        bind embedded [[], Configure]
          (BindSet([], (fun _ ->
           bind embedded [[], Configure] BindRemove;
           Frx_after.idle check;
              ()))))
          (Text.window_names cell)
         *)
      in
      let scroll, check = Fit.horiz cell gameover (
    let first_time = ref true in
    (fun () ->
      if !first_time then begin
        first_time := false;
        text_align cell align;
        if not nowrap then Text.configure cell [Wrap WrapWord];
        when_finished()
    end))
      in
      Text.configure cell [XScrollCommand scroll];
      check()
  | s ->
      if !debug then
    Log.f (sprintf "Table.dynamic_size: unknown children class %s" s);
(*e: function [[Table.dynamic_fight]] *)
      assert false


(*s: function [[Table.fixed_size]] *)
(* We know the size in pixels *)
let fixed_size cell width nowrap align =
  match Winfo.class_name cell with
  | "Text" ->
      if !debug then
    Log.f (sprintf "FIXED %s to %d" (Widget.name cell) width);
      if not nowrap then Text.configure cell [Wrap WrapWord];
      Fit.fixed_horiz cell width;
      (* we have to do alignment here, because it kills horizontal resizing *)
      text_align cell align;
      (* in all cases, we have to grow vertically *)
      let scroll, check = Fit.vert cell in
      Text.configure cell [YScrollCommand scroll];
      (* A posteriori updates for embedded windows
      List.iter 
    (fun embedded ->
      bind embedded [[], Configure]
        (BindSet([], (fun _ ->
          bind embedded [[], Configure] BindRemove;
          Frx_after.idle check;
             ()))))
        (Text.window_names cell);
       *)
      check()
  | s ->
      if !debug then
    Log.f (sprintf "Table.dynamic_size: unknown children class %s" s);
(*e: function [[Table.fixed_size]] *)
      assert false

(*s: function [[Table.sizing]] *)
(*
 * Determine how we should set resizing for our cells
 *  table.width contains the specified width for the table
 *  contextwidth was the width computed the context of the table
 *)
let sizing table nowrap width =
  (* For cells of given width and colspan 1, set a col minsize *)
  let colwidths = Array.create (Array.length table.slots) 0 in
  let setcolwidth col n =
    if n > colwidths.(col) then begin
      colwidths.(col) <- n;
      if !debug then
    Log.f (sprintf "%s col %d minsize %d"
               (Widget.name table.master_widget) col n);
      Grid.column_configure table.master_widget col [Minsize (Pixels n)]
    end
  in
  (* second pass to see if we have a proper column width for these *)
  let dynamic = ref [] in
  let add_dynamic w f = dynamic := (w,f) :: !dynamic in
  (* adjust sizes with padding/spacing *)
  let adjust w = w - 2 * table.cellspacing - 2 * table.cellpadding in
  (* Set initial size and dynamic resizing *)
  List.iter (function w,(_,col,_,cspan,cellwidth,_,align) -> 
    (* set initial width from images *)
    let initw = Fit.set_initial_width w
    (* set initial height from line number *)
    and _ = Fit.set_initial_height w in
    match cellwidth with
      FixedWidth n ->
    if cspan=1 then setcolwidth col n; (* this col is at least n*)
    fixed_size w (adjust n) nowrap align
    | UnknownWidth bound ->
    add_dynamic w bound
    | _ -> assert false)
    (List.rev table.slaves);
  (* second pass on dynamics : if we know exactly the size of the
     cell because we know exactly the size of each column it belongs to
     then set it *)
  List.iter (fun (w, f) ->
    let unknown_col = ref false
    and width = ref 0 in
    let (_,col,_,cspan,_,_,align) = List.assoc w table.slaves in
    for i = col to col + cspan - 1 do
      width := !width + colwidths.(i);
      if colwidths.(i) = 0 then unknown_col := true
    done;
    if not !unknown_col then fixed_size w (adjust !width) nowrap align
    else dynamic_fight w nowrap f align)
    !dynamic
(*e: function [[Table.sizing]] *)

(*s: function [[Table.packem]] *)
(* TODO: alignment *)
let packem table =
  let default_opts = [Sticky "nswe";
    PadX (Pixels table.cellspacing); PadY (Pixels table.cellspacing);
    IPadX (Pixels table.cellpadding); IPadY (Pixels table.cellpadding)]
  in
  List.iter 
    (fun (w, (row,col,rspan,cspan, _, _, _)) ->
       (* Sticky opt gives Expand true, Fill Both *)
       grid [w] ([Row row; Column col; RowSpan rspan; ColumnSpan cspan]
                 @default_opts))
    table.slaves
(*e: function [[Table.packem]] *)


(*s: function [[Table.get_slot]] *)
(* 
 * Slots represent, by column, the number of "pending" row-spanning cells 
 * If this number is zero, the slot is empty. When we allocate slots for
 * col-spanning cells, we keep these slots contiguous (case of overlapping
 * cells)
 *)

let get_slot table needed_cols rspan =
  (* First free slot in cur_col *)
  let rec first_free n =
    if n < Array.length table.slots then
      if table.slots.(n) = 0 then n
      else first_free (n+1)
    else raise Not_found in
  try 
    let first = first_free table.cur_col in
    (* Grow if overflow  (the next free would be first + needed_cols) *)
    if first + needed_cols > Array.length table.slots then
      table.slots <- 
         Array.append table.slots 
           (Array.create (first + needed_cols - (Array.length table.slots)) 
                         rspan);
    (* Mark used *)
    for i = first to first + needed_cols - 1 do
      table.slots.(i) <- max rspan table.slots.(i)
    done;
    table.cur_col <- first + needed_cols;
    first
  with
    Not_found -> (* Grow *)
      let first = Array.length table.slots in
      table.slots <- Array.append table.slots (Array.create needed_cols rspan);
      table.cur_col <- Array.length table.slots;
      first
(*e: function [[Table.get_slot]] *)
      

(*s: function [[Table.next_row]] *)
let next_row table =
  for i = 0 to Array.length table.slots - 1 do
    table.slots.(i) <- 
       match table.slots.(i) with
      0|1 -> 0
    | n -> n-1
  done
(*e: function [[Table.next_row]] *)


(*s: function [[Table.create]] *)
(*
 * The table manager 
 * [top] is the frame that will be embedded in the text widget
 *)


let create top tag contextwidth =
 let width = 
   try length_of_string (get_attribute tag "width")
   with Not_found -> Nolength
 and cellpadding =
   try int_of_string (get_attribute tag "cellpadding")
   with Not_found | Failure "int_of_string" -> 0 
 and cellspacing =
   try int_of_string (get_attribute tag "cellspacing")
   with Not_found | Failure "int_of_string" -> 0
 and bwidth =
   try int_of_string (get_attribute tag "border")
   with Not_found -> 0
      | Failure "int_of_string" -> 1
 and nowrap = has_attribute tag "nowrap"
 (* align attribute is ignored (flow of text) *)
 in
 Frame.configure top [BorderWidth (Pixels bwidth); Relief Raised];
 let tab = {
    master_widget = top;
    slaves = [];
    width = width;
    cur_col = 0;
    cur_row = -1; (* Start with TR *)
    slots = [||];
    cols = [];
    cellpadding = cellpadding;
    cellspacing = cellspacing} in

 (* Compute (if possible) the width of this table *)
 (* Set up the resize condition for cells of this table *)
 let size, bound = 
   match width with
     Nolength | LengthRel _ -> (* assume then 100% of context *)
       begin match contextwidth with
     TopWidth ->
       let w = topwidth tab.master_widget in
       None, Fit.bound_check tab.master_widget w
       | FixedWidth n -> (* size of parent cell *)
       Some n, Fit.bound_check tab.master_widget n
       | UnknownWidth f -> 
           (* the previous bound must have been reached
        * and we (the frame) may occupy 100% of the context 
        * (the text widget). Adjust to 95% for tuning.
        *)
       None, (fun () ->
         f() &&
           let w1 = Winfo.reqwidth top
           and w2 = Winfo.width (Winfo.parent top) in
           if !debug then
         Log.f (sprintf "Grow check %s=%d %s=%d"
                    (Widget.name top) w1
                    (Widget.name (Winfo.parent top)) w2);
            float w1 >= (float w2 *. 0.95))
       end
   | LengthPixels n -> 
       Some n, Fit.bound_check tab.master_widget n
   | LengthRatio r -> (* check the context *)
       begin match contextwidth with
     TopWidth ->
       let w = truncate (float (topwidth tab.master_widget) *. r) in
       Some w, Fit.bound_check tab.master_widget w
       | FixedWidth n -> (* size of parent cell *)
       let w = truncate (float n *. r) in
       Some w, Fit.bound_check tab.master_widget w
       | UnknownWidth f -> 
       None,
      (* the previous bound must have been reached,
         and we must occupy the ratio *)
       (fun () -> f()
           && 
         (let w1 = Winfo.reqwidth top in
          let w2 = Winfo.width (Winfo.parent top) in
           if !debug then
         Log.f (sprintf "Grow check %s=%d %s=%d"
                    (Widget.name top) w1
                    (Widget.name (Winfo.parent top)) w2);
          w1 >= truncate (r *. float w2)))
       end
  in
  (* SPECIAL FIX FOR THE PEOPLE WHO DON'T RESPECT THE DTD : we always make
     sure we are in a row *)
  let in_row = ref false in

   {table_master = top;
    bound = bound;
    close_table =
     (fun () ->
       packem tab;
       sizing tab nowrap size);
    add_col =  (fun tag -> 
      let span = 
       try int_of_string (get_attribute tag "span")
       with Not_found | Failure "int_of_string" -> 1 in
      let width = 
       (* Specification of the columns width (only pixel size supported) *)
       try Some (int_of_string (get_attribute tag "width"))
       with Not_found | Failure "int_of_string" -> None
      in 
      for i = 1 to span do
    tab.cols <- width :: tab.cols 
      done);

    open_row = (fun t ->
    tab.cur_col <- 0;
    tab.cur_row <- 1 + tab.cur_row;
        in_row := true;
        next_row tab);

    close_row = (fun () -> in_row := false);

    new_cell = (fun ctype attrs w align ->
      (* SPECIAL FIX FOR THE PEOPLE WHO DON'T RESPECT THE DTD *)
      if not !in_row then begin
    tab.cur_col <- 0;
    tab.cur_row <- 1 + tab.cur_row;
        in_row := true;
        next_row tab
    end;
      let opts = match ctype with
     HeaderCell -> [Relief Groove]
       | DataCell -> [Relief Sunken]
      in
      begin match Winfo.class_name w with
      |	"Text" -> Text.configure w opts
      |	_ -> assert false
      end;
      (* Tk needs spans > 0 *)
      let rspan = 
    try max 1 (int_of_string (get_attribute attrs "rowspan")) 
    with Not_found | Failure "int_of_string" -> 1
      and cspan = 
    try max 1 (int_of_string (get_attribute attrs "colspan"))
    with Not_found | Failure "int_of_string" -> 1
      and width = 
    try length_of_string (get_attribute attrs "width")
    with Not_found -> Nolength
      and height = 
    try length_of_string (get_attribute attrs "height")
    with Not_found -> Nolength
      in
      (* find its place *)
      let real_col = get_slot tab cspan rspan in
      if !debug then
          Log.f (sprintf "Cell %s at row=%d col=%d rspan=%d cspan=%d"
                 (Widget.name w)
                     tab.cur_row real_col rspan cspan);
      (* compute the size of this cell, so that tables in it have 
     something to work on *)
      let wconstraint = match width with
       Nolength | LengthRel _ -> UnknownWidth bound
      |	LengthPixels n -> FixedWidth n
      |	LengthRatio r -> 
      if !strict_32 then UnknownWidth bound
      else
        (* variable size : do we know the size of the table ? *)
      match size with
        None -> UnknownWidth (fun () ->
          bound() &&
           let w1 = Winfo.reqwidth w
           and w2 = Winfo.width top in
           w1 >= truncate (float w2 *. r))
      | Some n -> FixedWidth (truncate (float n *. r))
      in
       (* We delay the gridding until we have all cells *)
       tab.slaves <- 
          (w, (tab.cur_row, real_col,
           rspan, cspan,
           wconstraint, height, align))
         :: tab.slaves;
       wconstraint
       )}
(*e: function [[Table.create]] *)
(*e: display/table.ml *)

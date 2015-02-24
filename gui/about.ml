(*s: ./gui/about.ml *)
open Tk

(*s: function About.f *)
let f () =
  ignore (
    Frx_dialog.f Widget.default_toplevel (Mstring.gensym "about")
    "About MMM" (Version.about (Lang.lang ()))
    (Tk.Predefined "info") 0 ["Thanks"])
(*e: function About.f *)


(* Tachymeter *)

(* gif is 80x65 *)
(*s: constant About.tachy_data *)
(* inside bitmap, circle is in +16+7 +66+57, radius 25 *)

let tachy_data = "GIF\056\057aP\000A\000\227\000\000\000\000\000\
\044\044\044\060\000\000YYY\138\138\138\154\154\154\170\
\170\170\186\186\186\203\203\203\219\219\219\231qq\235\235\
\235\243yy\255\255\255\000\000\000\000\000\000\033\249\004\
\001\000\000\009\000\044\000\000\000\000P\000A\000\000\004\
\255\048\201I\171\189\024H\157\056\199\096\040\142\164\213\
Hg\146\166e\235\190p\060\013\052\045\223DN\000\
M\225\223\148\001\133\005\020\233\142\187\158oY\184\009\
\039\196\162\005I\229\049\153\006\131E\032z\162\164\023\
\042\178\144\188\250\178\232J\180\226U\021\063\146\129\248\
\136\174c\235\233\009\220\212\232\175a\041\052sx\132\
\133\133P\096\033p\130H\134\142\143\006\007z\137\032\
\129rt\144\153x\007\156\136\148\147\051\151\058\154\164\
Y\156\156\004\160\159n\009\003\000\152\165\164\167I\158\
\171\173r\175\004\177\187\007I\157\018\092\171\140\059\187\
\197\060\167\146\095\148\195\186\197\197\200\008\008\182\173\176\
\206\188\007\209\210\194\213\214\187\217\209\171\141\221\206\223\
\218\137\226\227\228\223\148\220\233\222\217\231\237\238\165\229\
\096\242\143\001\001\003\001\006\174\053\006\249\000\210\131\039\
\229\158\035\125\253\250\001\232\055\064\033\141\129\230\054\196\
\064G\010\192\190\044\251\030fA\056\240\194\031\035\163\
\255b\185J\008\208\031\198\128\177\216\133\044\165\049\203\
\194\135\015\045\166\180\176\167\132AC\045\255\213\216\185\
\203\227\011\138\243\172\197\203\017t\030\205\018\031n\022\
\037u\164\150\136\020J\151f\002\064\181f\139\170\011\
\221\037\152\231\039\133\213\011\031\186J\029\167\160\172\002\
\167\033\160\046\025\219\205\236Y\025w\216\022s\043\241\
\197\021\185\187\220\190e\229\034\046\094Mt\129\248\253\
\011\041p\135\024\131\009\059\050\204\183\197\093\197\143\220\
\050\144\178\022r\228\178\147\041\023\176\092X\001\131\204\
E\054s\094\236\249s\162\209\139\063\131\150\130\186P\
i\211\148Z\215y\189\026\140l\052\170a\127\186M\
\123Z\029N\144\049\171\158\182\053R\175\172x\001\044\
\016N\092\002\170cx\023\044P\094\182\249\004\095\192\
\199J\151\174\220\250\016d\217\231m\031\239\253\176s\
\240\008\196\143\151\190\225k\034\022\208\178u\091\191\125\
\210G\091\156\202\201\039E\159\252\144\251\211\232\183N\
\045qxH\208\095\125\229\149\032\096\005\180Lp\096\
\130\048\012HA\131\009\244\007a\056\060\024\232\223\133\
\024\158\192\030\135\032\134\040\226\136\036\194\016\001\000\059\
"
(*e: constant About.tachy_data *)
(*s: constant About.park_data *)
let park_data =
"#define break_width 15
#define break_height 11
static char break_bits[] = {
   0x0c, 0x18, 0xf4, 0x17, 0x3a, 0x2e, 0xba, 0x2d, 0xb9, 0x4d, 0x3d, 0x5e,
   0xb9, 0x4f, 0xba, 0x2f, 0xba, 0x2f, 0xf4, 0x17, 0x08, 0x08};
"
(*e: constant About.park_data *)


(*s: constant About.pi *)
let pi = 3.1415926 
(*e: constant About.pi *)
(*s: constant About.log10 *)
let log10 = log 10.0 
(*e: constant About.log10 *)

class default_tachy (top : Widget.widget) =
 object (self)
  (* val top = top *)
  val mutable canvas = top (* dummy initialisation *)
  val mutable alive = false

  (* Various components of the canvas, all with dummy init values *)
  val mutable i_park = Tag "none"
  val mutable kilos = Tag "none"
  val mutable aig = Tag "none"
  val mutable pendings = Tag "none"


  (* this one is private *)
  method start =
    let c =
      Canvas.create_named top "tachymeter"
       [Width (Pixels 80); Height (Pixels 80); 
         BorderWidth (Pixels 0);
         HighlightThickness (Pixels 0);
         TakeFocus true (* pl3 fix *)] in
    (* Use colors so that images are not transparent *)  
    let tachy_image = 
      begin
       try
      let bgc = Tk.cget c CBackground in
           Protocol.tkCommand 
        [|Protocol.TkToken "set";
          Protocol.TkToken "TRANSPARENT_GIF_COLOR";
          Protocol.TkToken bgc |]
       with _ -> ()
      end;
    (* Agghaaa !!! TCL/TK doesn't support -data for GIF !!! *)
      let file = Msys.mktemp "tachy.gif" in
      let oc = open_out_bin file in
      output_string oc tachy_data;
      close_out oc;
      let img = Imagephoto.create [File file] in
      Msys.rm file;
      img
    and park_image =
      Imagebitmap.create [Data park_data; Foreground Red] in

    i_park <-
      Canvas.create_rectangle c 
     (Pixels 72) (Pixels 3) 
     (Pixels 75) (Pixels 6) [FillColor Black];
    
    kilos <-
      Canvas.create_text c (Pixels 40) (Pixels 73) [Text "0"];
    
    aig <-
      Canvas.create_line c [Pixels 41; Pixels 32; Pixels 41; Pixels 57]
                       [Width (Pixels 2)];
    pendings <-
      Canvas.create_text c (Pixels 70) (Pixels 60) [Text "0"];

    let i_tachy =
      Canvas.create_image c (Pixels 0) (Pixels 0)
       [ImagePhoto tachy_image; Anchor NW]

    in

    Canvas.lower_bot c pendings;
    (* All other items must be put above the background image *)
    List.iter (fun i -> Canvas.raise_above c i i_tachy)
      [kilos; aig; i_park];

    bind c (Glevents.get "tachy_about") (BindSet ([], (fun _ -> f ())));

    bind c (Glevents.get "tachy_gc") (BindSet ([], (fun _ -> Frx_mem.f())));

    bind c [[], Destroy] (BindSet ([], (fun _ -> alive <- false)));

    pack [c][];
    alive <- true;
    canvas <- c

  val mutable last_speed = 0.
  val mutable last_total = 0
  val mutable idle = false

  method update speed total =
    if speed = 0.0 then begin
      if not idle then begin
    Canvas.configure_rectangle canvas i_park [FillColor Black;
                          Outline Black];
    idle <- true
      end
    end
    else begin
      Canvas.configure_rectangle canvas i_park [FillColor Green;
                         Outline Green];
      idle <- false
    end;
    if total <> last_total then
      Canvas.configure_text canvas kilos [Text (string_of_int total)];
    last_total <- total;
    let speed = if speed = 0. then 0. else log speed in
       (* Smooth *)
    let speeds = (last_speed +. speed) /. 2. in
    if abs_float (speeds -. last_speed) > 0.1 then begin
      last_speed <- speeds;
      let v = speeds /. log10 in
      let angle = v /. 4.0 *. pi in
      let angle = if angle < 0.1 then 0.0 else angle in
      let x = 41.0 -. (sin angle *. 25.0)
      and y = 32.0 +. (cos angle *. 25.0) in
      Canvas.coords_set canvas aig 
    [Pixels 41; Pixels 32;
      Pixels (truncate x); Pixels (truncate y)];
      Low.update_idletasks()
    end

  method report_cnx n = 
    if Winfo.exists canvas then
      if n = 0 then begin
    Canvas.configure_text canvas pendings [Text ""];
       Canvas.lower_bot canvas pendings
      end
      else begin
    Canvas.configure_text canvas pendings 
      [Text (string_of_int n)];
       Canvas.raise_top canvas pendings
      end

  method report_busy busy =
    if Winfo.exists canvas then
      if busy then begin
       Canvas.lower_bot canvas pendings;
    Canvas.configure_rectangle canvas i_park [FillColor Red;
                          Outline Red];
    Low.update_idletasks()
      end
      else begin
       Canvas.raise_top canvas pendings;
    Canvas.configure_rectangle canvas i_park [FillColor Black;
                          Outline Black]
      end

  method report_traffic tick_duration bytes_read sample_read =
    if alive then
      self#update (float sample_read *. 1000. /. float tick_duration)
    bytes_read

  method quit =
    alive <- false;
    destroy canvas

end

(*s: function About.create_tachy *)
let create_tachy top = 
  let o = new default_tachy top in
  o#start;
(*e: function About.create_tachy *)
  (o :> Low.tachymeter)

(*e: ./gui/about.ml *)

open Safe418mmm
open Tk

module Provide = struct
  let capabilities = Capabilities.get()
  end

module Mmm = Get(Provide)

(* Tachymeter *)

(* gif is 58x60 *)
(* inside bitmap, circle is in +10+6 +47+43, radius 18.5 *)

let tachy_data = "GIF\056\055a\058\000\060\000\165\000\000\168\168\168\
\168\152\176\152\152\168\152\152\152\152\136\160\136\152\152\136\
\136\152\128\136\128\128t\152\144\172\200\136\152\184\144\132\
\168\128\132\152\136\136\136\000\000\000\248\252\248\216\216\216\
\040\040\040p\136\136\184\184\184\200\200\200XXX\128\
txht\152pt\136\040\044\040ptphd\
\144\232\232\232\096d\136\224pp\240xx\096dp\
\096\096h\096TpPdpHXxXTX\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\044\000\000\
\000\000\058\000\060\000\000\006\254\064\128\016\016\032\026\139\
\001\001R\201\020\052\007\004\168\148P\160Z\013W\002\
\214\096\056p\187\136\195\016\144\024\011\145\000\005\064\176\
n\179\005\010\167\160\048\159\091\011\139\042\190\096\224\243\
\191\006\012\129\006gCh\135JDJ\003I\002\003\
\142\142QVPzY\091\093\152\007aae\134G\
FjEjr\002\013\166\167\014\015\003\003\166\016\149\
\123\095\127\092\017\180\180\018\008\135G\137I\188N\172\
\167\192\013\169\171\196\019\020\174Z\092\201\094\179\155\018\
\016bf\159\163n\193\214\195\196\003c\016\125\221\128\
\018\017\130\183\220\185\136\189\214\193\019c\196\210\016\220\
\153\096\017\155\007\016\018\159h\161m\021\214\171\210\254\
\237\128\004\025\136pk\028\046\093Kx\057\216\215\224\
\215\191\135f\038L\128\006F\211\051w\238\236\009\201\
G\141\205\062\134\013\033\138\148\056\193A\061\003\018P\
\034\040\184\242\160\057\038\251\022\134\020\057\178d\003w\
\154r\174\180\032\129\231\254\180\053N\000\124\020V\129\
\166Q\000\013\000H\172\215\146\165\132\011\188\020\245\026\
j\234\232Q\137\020\142\021\228\185\019\003\040\055k\128\
i\179j\212XV\008MW\094\144\128\033a\147\096\
d\173f\061\203\179\046\006\011\094G\005u\034\054\174\
\220\179\005\215b\192\000\149I\163\095\167\252Z\053\155\
\017\239\096\188\096\229\000\051\154\161B\006\162E\043g\
\016\057\023\130\096\182l\123AB\156\148f\209\152B\
\133V\230\124\204\029\094\199\026\246J\062\053\022b\131\
\213\183\047\035\205\160\014\226D\140\131\217\014\062\012i\
\178\233\210\167\027TX\094T\036F\008\176\055x\037\
\053\199\184\200\125B\150\011e\222\220\057\004\014\194\007\
c\024\029\201\186b\185\192\053\096P\031\091\014\029\005\
\180\207\251\061\037\030C\007\012Q\034Aq\192\191\182\
\252\163\252\057\032\221\006\026\016\232\004\029\008\146T\026\
Y\020\040F\210\096\234\221\167\159\021\166\248\247\159Q\
\030d\184\030\129\004\254\190gG\001\236\092hU\134\
\030\220w\095\129\146T\146\141\136F\125\144\225\007\028\
\022\168\001\130w\172\200\162H\030\124\160\035\123\004\226\
\247J\020\033\222\248\208\139\048\202\216\225\029\147\244\035\
\228\063\057\234\184\161\145Hb\001\162\133K\142A\164\
\145\026h\000\130\030yP\177\128\146U\154\049\064\147\
E\174\215\001\138\063bAe\152\058\234\024c\150Y\
\190\146\007\023aJ\211\230\142\049\018\216\129\022\124\126\
\161E\131u\010q\167\140Zn\000\130\150\092\250\209\
\013\004CH\180\036\153\031\152\169\193\153g\130\096\201\
\050\006\048\042\145r\055\054\064\036\129Z\194y\232\150\
s\002\002\008\004\019\008\195\168\136\015\056\224b\145\049\
V\010\130\165\126r\161\147\038\020\152D\146\124\020p\
\224\171\171\160\142\170\229\172\179\202\194\135\064\040\037\139\
\042I\128\198\229\235\003\190B\160\231\164\212\018\171\037\
\001\007\044\019FE\059\045kV\179\052\245\234\235\184\
\193\134\042\236\172\254\033\152\250EJ\226\180\004\193\092\
\240be\198\092\227\214\203\193\180\211\018\171\239\023\183\
\058\211\020\094\238\192\219\171\059\037\061\128\145\189\028\064\
\064\232\185\250\130\016\130\008\233\014\034N\178N\053u\
\193\187\024cT\176\059\245\026\028\043\181\178\018\059\002\
\008\035W\196mO\091\161\252\218\093\024\060\183q\189\
\160\194i\238\176\163\058\012\177\195\223\012Rq\096\225\
\133\055\152\137O\230\009\050\008\033\207\058\178\209\034\204\
\227\239V\044\059\198\242\134\016\010\045\244\204\013\219\252\
\240\213\019\167\228\212Zj\129\134\129\207\246\133\045c\
\208\213\154\171\239\200h\147\012\194\201\092\245t\151\219\
\043\175\039\055\150o\202\204\176\190W\091\045\194\008\005\
\169\180\179\096\245\213\007\052\221\050RJ\052\205F\139\
\188\055\201I\091\244o\211\226E\135\129\212\132\046l\
\054\222\016g\254\240\008\123km\049h\128\127\029\054\
\132b\247\136e\165\151\039\158\054\218\123\039\141rW\
\043\199\061\054j\225\049\139\154\058\186\154g\206\249\230\
\059w\029\184\224\165\019\046k\209j\035\205\058\231\035\
\144\240x\220\245\209\142e\161YZ\091\245\213\173o\
\222\250\008\033\148\192\179\232\096\015\062\183\233\031\223\173\
\058\227\199\239MB\242\176C\030\029\143\132\203L\181\
\195\196RO\061\242\123g\159\253S\219\255N\250\137\
\148\135\140\184\218\229\163\095\242\146W\002\018\004\001\000\
\059\
"

let park_data =
"#define break_width 15
#define break_height 11
static char break_bits[] = {
   0x0c, 0x18, 0xf4, 0x17, 0x3a, 0x2e, 0xba, 0x2d, 0xb9, 0x4d, 0x3d, 0x5e,
   0xb9, 0x4f, 0xba, 0x2f, 0xba, 0x2f, 0xf4, 0x17, 0x08, 0x08};
"


let pi = 3.1415926 
let log10 = log 10.0 

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
      	[Width (Pixels 56); Height (Pixels 60); 
      	  BorderWidth (Pixels 0);
      	  HighlightThickness (Pixels 0);
      	  TakeFocus true (* pl3 fix *)] in
    (* Use colors so that images are not transparent *)  
    (*
    let tachy_image = 
      begin
      	try
	  let bgc = Tk.cget c CBackground in
	  Protocol.tkEval 
	    [|Protocol.TkToken "set";
	      Protocol.TkToken "TRANSPARENT_GIF_COLOR";
	      Protocol.TkToken bgc |]; () 
      	with _ -> ()
      end;
      *)
    let tachy_image = Frx_misc.create_photo [Data tachy_data]
    and park_image =
      Imagebitmap.create [Data park_data; Foreground Red] in

    i_park <-
      Canvas.create_rectangle c 
    	(Pixels 50) (Pixels 4) 
    	(Pixels 53) (Pixels 7) [FillColor Black];
	
    kilos <-
      Canvas.create_text c (Pixels 28) (Pixels 52) [Text "0"; Font "-adobe-helvetica-medium-r-*-*-8-*-*-*-*-*-*-*"];
    
    aig <-
      Canvas.create_line c [Pixels 27; Pixels 25; Pixels 27; Pixels 43]
	                   [Width (Pixels 2)];
    pendings <-
      Canvas.create_text c (Pixels 52) (Pixels 39) [Text "0"; Font "-adobe-helvetica-medium-r-*-*-8-*-*-*-*-*-*-*"];

    let i_tachy =
      Canvas.create_image c (Pixels 56) (Pixels 0)
      	[ImagePhoto tachy_image; Anchor NE]

    in

    Canvas.lower_bot c pendings;

    (* All other items must be put above the background image *)
    List.iter (fun i -> Canvas.raise_above c i i_tachy)
      [kilos; aig; i_park];

    bind c [[], Destroy] (BindSet ([], (fun _ -> alive <- false)));

    (* These bindings are specific to the applet version *)
    bind c [[], ButtonPressDetail 1] 
      (BindSet ([], (fun _ -> Mmm.new_window_initial (); ())));
    bind c [[], ButtonPressDetail 2] 
      (BindSet ([], (fun _ -> Mmm.new_window_sel (); ())));

    alive <- true;
    pack [c][];
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
      let x = 27.0 -. (sin angle *. 18.5)
      and y = 25.0 +. (cos angle *. 18.5) in
      Canvas.coords_set canvas aig 
	[Pixels 27; Pixels 25;
	  Pixels (truncate x); Pixels (truncate y)];
      update_idletasks()
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
	update_idletasks()
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

let create_tachy top = 
  let o = new default_tachy top in
  o#start;
  (o :> Mmm.tachymeter)

let _ = 
  let top = Applets.get_toplevel_widget [] in
  Wm.withdraw top;
  begin match Frx_dialog.f top (Mstring.gensym "foo")
        "Tachy test" "Use this aftermmm tachymeter"
	(Tk.Predefined "question") 1 ["Yes"; "No"] with
    0 -> Mmm.set_tachy create_tachy
  | _ -> ()
  end;
  destroy top


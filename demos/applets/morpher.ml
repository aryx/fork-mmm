open Safe418

module type RawGfx = sig
  type t
  type pixel
  val create : int -> int -> t        (* [create_pixmap w h] *)
  val copy : t -> t -> unit      (* [copy_pixmap from to] *)
  val get_pixel: t -> int -> int -> pixel    (* [get_pixel pixm x y]  *)
  val set_pixel: t -> int -> int -> pixel -> unit
  val default_color : pixel
  val from_file : string -> t
  val width : t -> int
  val height : t -> int
end


type point = { x : float ; y : float }


(* Compute vect(v1) + vect(v2) *)
let add v1 v2 = { x = v1.x +. v2.x; y = v1.y +. v2.y}
let sub v1 v2 = { x = v1.x -. v2.x; y = v1.y -. v2.y}
let mul s v = { x = v.x *. s; y = v.y *. s}

(* Compute vect(v1) . vect (v2) *)
let prod_scal v1 v2 = v1.x *. v2.x +. v1.y *. v2.y



let vect p1 p2 = sub p2 p1

module Make(G : RawGfx) = struct

open G

let big = 1e+42
let (/.) = fun x y ->
  if y = 0.0 then big else x /. y

let morph pix_from pix_to o s =
  G.copy pix_from pix_to;
  if o <> s then begin
    let w = G.width pix_from
    and h = G.height pix_from in
    let os_ = vect o s in
    let os_ = mul 0.2 os_ in
    let s = add o os_ in
    let f = add s (vect o s) in
    let e = add o (vect f o) in
    let y = {x = s.x -. os_.y; y = s.y +. os_.x} in
    let sy = vect s y in
    let sy2 = prod_scal sy sy in
    let se = vect s e in
    let sf = vect s f in
    let so = vect s o in
    let sesf = prod_scal se sf in
    let sosf_sy2 = -. prod_scal so sf /. sy2 in
    let sf2 = prod_scal sf sf in
    
    (* bounding box *)
    let c = add y sf in
    let b = add y se in
    let z = sub s sy in
    let d = add z sf in
    let a = add z se in
    let xmax = truncate (max (max a.x b.x) (max c.x d.x)) in
    let ymax = truncate (max (max a.y b.y) (max c.y d.y)) in
    let xmin = truncate (min (min a.x b.x) (min c.x d.x)) in
    let ymin = truncate (min (min a.y b.y) (min c.y d.y)) in
    (* For each point of the (bounded) destination image... *)
    for xi = max xmin 0 to min xmax (w - 1) do
      for yi = max ymin 0 to min ymax (h - 1) do
      	let m1 = { x = float xi ; y = float yi } in
	let sm1 = vect s m1 in
      	let sm1sy = prod_scal sm1 sy in
      	let sm1sf = prod_scal sm1 sf in
        (* Test if we are in the warped square *)
      	if -. sy2 <= sm1sy && sm1sy <= sy2 && sesf <= sm1sf && sm1sf <= sf2
      	then (* Yes we are... *)
        (* Test if we are in the compressed or stretched part, i.e on which *)
        (* side of the limit-line. *)
	  if -. sm1sf /. abs_float (sm1sy) <= sosf_sy2 then begin
          (* Compress *)
	    let dist = sm1sy /. sy2 in
            let fr = mul dist sy in
            let r = add f fr in
            let rm1 = vect r m1 in
            let pp = abs_float dist in
            let scale = pp +. (1.0 -. pp) *. 0.5 in
            let rm = { x = rm1.x /. scale; y = rm1.y /. scale } in
            let m = add r rm in
            let mxi = truncate m.x in
            let myi = truncate m.y in
            let color = 
	      if mxi < 0 || mxi >= w || myi < 0 || myi >= h then default_color
	      else get_pixel pix_from mxi myi in
            set_pixel pix_to xi yi color
          end
	  else begin
            (* Stretch *)
	    let dist = sm1sy /. sy2 in
            let ep = mul dist sy in
            let p = add e ep in
            let pm1 = vect p m1 in
            let pp = abs_float dist  in
            let scale = pp +. (1.0 -. pp) *. 1.5 in
            let pm = { x = pm1.x /. scale; y = pm1.y /. scale } in
            let m = add p pm in
            let mxi = truncate m.x in
            let myi = truncate m.y in
            let color = 
	      if mxi < 0 || mxi >= w || myi < 0 || myi >= h then default_color
	      else get_pixel pix_from mxi myi in
            set_pixel pix_to xi yi color
          end
      done
   done
 end
end



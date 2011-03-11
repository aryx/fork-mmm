(* Tk_GetPixels emulation *)
(* type *)
type units =
   | Pixels of int       (* specified as floating-point, but inconvenient *)
   | Centimeters of float
   | Inches of float
   | Millimeters of float
   | PrinterPoint of float
(* /type *)

let cCAMLtoTKunits = function
  | Pixels u -> TkToken (string_of_int u)
  | Millimeters u -> TkToken (string_of_float u ^ "m")
  | Inches u -> TkToken (string_of_float u ^ "i")
  | PrinterPoint u -> TkToken (string_of_float u ^ "p")
  | Centimeters u -> TkToken (string_of_float u ^ "c")

let cTKtoCAMLunits str =
  let len = String.length str in
  let num_part str = String.sub str 0 (len - 1) in
  match String.get str (pred len) with
  | 'c' -> Centimeters (float_of_string (num_part str))
  | 'i' -> Inches (float_of_string (num_part str))
  | 'm' -> Millimeters (float_of_string (num_part str))
  | 'p' -> PrinterPoint (float_of_string (num_part str))
  | _ -> Pixels(int_of_string str)

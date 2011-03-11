(* Tk_GetBitmap emulation *)
(* type *)
type bitmap =
   | BitmapFile of string                 (* path of file *)
   | Predefined of string                 (* bitmap  name *)
(* /type *)

let cCAMLtoTKbitmap = function
  | BitmapFile s -> TkToken ("@" ^ s)
  | Predefined s -> TkToken s

let cTKtoCAMLbitmap s = 
  if s = "" then Predefined "" else
  if String.get s 0 = '@'
  then BitmapFile (String.sub s 1 (String.length s - 1))
  else Predefined s

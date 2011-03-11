(* type *)
type paletteType =
   | GrayShades of int
   | RGBShades of int * int * int
(* /type *)

let cCAMLtoTKpaletteType = function
  | GrayShades g -> TkToken (string_of_int g)
  | RGBShades (r, v, b) ->
      TkToken
        (String.concat "/" [string_of_int r; string_of_int v; string_of_int b])























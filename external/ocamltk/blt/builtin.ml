external init : unit -> unit = "blt_init"

type cellIndex = int * int

let cCAMLtoTKcellIndex (x,y) =
  TkToken(string_of_int x ^ "," ^ string_of_int y)

type dataType = string

let cCAMLtoTKdataType x = TkToken x


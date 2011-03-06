open Tk

let vgroup top l =
  let f = Frame.create top [] in
    Pack.forget l;
    Pack.configure l [In f];
  f

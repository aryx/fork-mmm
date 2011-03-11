let create parent title mesg bitmap def buttons =
  let w = Widget.new_atom "toplevel" parent in
  let res =
    tkEval [|
      TkToken"tk_dialog";
      cCAMLtoTKwidget widget_any_table w;
      TkToken title;
      TkToken mesg;
      cCAMLtoTKbitmap bitmap;
      TkToken (string_of_int def);
      TkTokenList (List.map (function x -> TkToken x) buttons)
    |] in
  int_of_string res

let create_named parent name title mesg bitmap def buttons =
  let w = Widget.new_named "toplevel" parent name in
  let res =
    tkEval [|
      TkToken"tk_dialog";
      cCAMLtoTKwidget widget_any_table w;
      TkToken title;
      TkToken mesg;
      cCAMLtoTKbitmap bitmap;
      TkToken (string_of_int def);
      TkTokenList (List.map (function x -> TkToken x) buttons)
    |] in
  int_of_string res

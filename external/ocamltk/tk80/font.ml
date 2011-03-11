open Protocol
open Tk
open Widget
open Textvariable
let actual v1 v2 =
  let res = tkEval [|
        TkToken "font";
        TkToken "actual";
        cCAMLtoTKfont v1;
        TkTokenList (
        List.map (function x -> cCAMLtoTKfontOptions x )
           v2
        )
        |] in 
  res

let actual_displayof v1 v2 v3 =
  let res = tkEval [|
        TkToken "font";
        TkToken "actual";
        cCAMLtoTKfont v1;
        TkToken "-displayof";
        cCAMLtoTKwidget widget_any_table v2;
        TkTokenList (
        List.map (function x -> cCAMLtoTKfontOptions x )
           v3
        )
        |] in 
  res

let configure v1 v2 =
  tkCommand [|
        TkToken "font";
        TkToken "configure";
        cCAMLtoTKfont v1;
        TkTokenList (
        List.map (function x -> cCAMLtoTKfontOptions x )
           v2
        )
        |]

let create v1 =
  let res = tkEval [|
        TkToken "font";
        TkToken "create";
        TkTokenList (
        List.map (function x -> cCAMLtoTKfontOptions x )
           v1
        )
        |] in 
  cTKtoCAMLfont res

let delete v1 =
  tkCommand [|
        TkToken "font";
        TkToken "delete";
        cCAMLtoTKfont v1
        |]

let failsafe v1 =
  tkCommand [|
        TkToken "font";
        TkToken "failsafe";
        TkToken v1
        |]

let families () =
  let res = tkEval [|
        TkToken "font";
        TkToken "families"
        |] in 
  (splitlist res)

let families_displayof v1 =
  let res = tkEval [|
        TkToken "font";
        TkToken "families";
        TkToken "-displayof";
        cCAMLtoTKwidget widget_any_table v1
        |] in 
  (splitlist res)

let measure v1 v2 =
  let res = tkEval [|
        TkToken "font";
        TkToken "measure";
        cCAMLtoTKfont v1;
        TkToken v2
        |] in 
  int_of_string res

let measure_displayof v1 v2 v3 =
  let res = tkEval [|
        TkToken "font";
        TkToken "measure";
        cCAMLtoTKfont v1;
        TkToken "-displayof";
        cCAMLtoTKwidget widget_any_table v2;
        TkToken v3
        |] in 
  int_of_string res

let metrics v1 v2 =
  let res = tkEval [|
        TkToken "font";
        TkToken "metrics";
        cCAMLtoTKfont v1;
        TkToken v2
        |] in 
  int_of_string res

let metrics_displayof v1 v2 v3 =
  let res = tkEval [|
        TkToken "font";
        TkToken "metrics";
        cCAMLtoTKfont v1;
        TkToken "-displayof";
        cCAMLtoTKwidget widget_any_table v2;
        TkToken v3
        |] in 
  int_of_string res

let metrics_linespace v1 =
  let res = tkEval [|
        TkToken "font";
        TkToken "metrics";
        cCAMLtoTKfont v1;
        TkToken "-linespace"
        |] in 
  int_of_string res

let names () =
  let res = tkEval [|
        TkToken "font";
        TkToken "names"
        |] in 
  (splitlist res)


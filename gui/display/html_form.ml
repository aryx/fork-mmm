(*s: display/html_form.ml *)

(*
 * Level 2 stuff (forms)
 *)

(* The behaviour of a form *)

class behaviour (base, formtag, deftarget, i18n_encoder) =
  object (self)
    val mutable (*private*) elem_value
        : (Htmlfmt.input_kind * (unit -> (string * string) list)) list =
      []

    val mutable (*private*) elem_reset : (unit -> unit) list = []

    val (*private*) fmethod =
      String.uppercase_ascii (Html.get_attribute formtag "method")

    val (*private*) encoding = Html.get_attribute formtag "enctype"

    (* val i18n_encoder = i18n_encoder *)
    val (*private*) action =
      try Html.get_attribute formtag "action" with
      | Not_found -> base

    (* val base = base *)
    val (*private*) h_params =
      try [ ("target", Html.get_attribute formtag "target") ] with
      | Not_found -> (
          match deftarget with
          | Some s -> [ ("target", s) ]
          | None -> [])

    val mutable (*private*) entries = 0 (* number of text entries *)

    (* Contribute a value to the form *)
    method add_get kind f =
      elem_value <- (kind, f) :: elem_value;
      if kind = EntryInput then entries <- entries + 1

    (* How to reset the element *)
    method add_reset f = elem_reset <- f :: elem_reset

    (* The link for a given submit activation *)
    method submit (l : (string * string) list) : Hyper.link =
      let values =
        List.flatten
          (List.map
             (function
               | _, f -> f ())
             elem_value)
      in
      let values = l @ values in
      (* These values must be encoded in the same Kanji code of the source here,
       * if it is Japanese mode. And the difficulty is the Kanji code of the
       * document is usually lazily determined. --- JPF 
       *)
      let values_i18n = List.map (fun (t, v) -> (t, i18n_encoder v)) values in
      let evalues = Urlenc.form_encode (List.rev values_i18n) in
      match fmethod with
      | "POST" ->
          Hyper.
            {
              h_uri = action;
              h_context = Some base;
              h_method = POST evalues;
              h_params;
            }
      | _ ->
          let uri =
            let l = String.length action in
            if l = 0 then Printf.sprintf "?%s" evalues
            else if action.[l - 1] = '?' then action ^ evalues
            else Printf.sprintf "%s?%s" action evalues
          in
          { h_uri = uri; h_context = Some base; h_method = GET; h_params }

    (* Submit if only one entry in the form. This may not be the proper test. *)
    method single_submit = if entries = 1 then Some (self#submit []) else None

    (* Resetting the form *)
    method reset = List.iter (fun f -> f ()) elem_reset
  end

(*
module Make(FormDisplay : FormDisplay) = 
 struct
  open FormDisplay
*)
module FormDisplay = Form
(*
 * <!ELEMENT FORM - - %body.content -(FORM)>
 * <!ATTLIST FORM
 *         action %URL #REQUIRED -- server-side form handler --
 *         method (%HTTP-Method) GET -- see HTTP specification --
 *         enctype %Content-Type; "application/x-www-form-urlencoded"
 *         >
 *)

let init mach =
  mach#add_tag "form"
    (fun _fo tform ->
      let behav =
        new behaviour (mach#base, tform, mach#target, mach#i18n_encoder)
      in
      let fm = FormDisplay.create mach#base behav mach#ctx in

      (* 8.1.2 Input Field : INPUT *)
      let open_input (fo : Htmlfmt.formatter) t =
        let inputtype = String.uppercase_ascii (Html.get_attribute t "type") in
        (* Special case for hidden, since there is no formatting *)
        (* HTML 3.2 doesn't specify that NAME and VALUE are required, but
           this is stupid *)
        if inputtype = "HIDDEN" then begin
          try
            let name = Html.get_attribute t "name" in
            let v = Html.get_attribute t "value" in
            behav#add_get OtherInput (fun () -> [ (name, v) ])
          with
          | Not_found ->
              raise (Html.Invalid_Html "missing NAME or VALUE in input HIDDEN")
        end
        else
          (* Other cases *)
          let fr =
            fo.create_embedded (Html.get_attribute t "align") None None
          in
          match inputtype with
          | "TEXT"
          | "PASSWORD" ->
              fm.text_input fr t
          | "CHECKBOX" -> fm.checkbox_input fr t
          | "RADIO" -> fm.radio_input fr t
          | "IMAGE" ->
              let caps = Cap.network_caps_UNSAFE () in
              mach#imgmanager#add_image caps (fm.image_input fr t)
          | "SUBMIT" -> fm.submit_input fr t
          | "RESET" -> fm.reset_input fr t
          (* TODO: file *)
          | s -> raise (Html.Invalid_Html ("Invalid INPUT TYPE=" ^ s))
      in
      mach#add_tag "input" open_input (fun _ -> ());

      (* 8.1.3 Selection : SELECT *)
      (* the /SELECT does all the job, so we have to transmit the info ! *)
      let options = ref [] (* the components from which to select *)
      and tselect = ref Html.{ tag_name = "select"; attributes = [] } in
      let open_select _fo t =
        options := [];
        tselect := t;
        mach#add_tag "option"
          (fun _fo tag ->
            mach#push_action (fun s ->
                let s = Html.beautify2 s in
                (* the val is by default the "content" of the tag *)
                let va =
                  try Html.get_attribute tag "value" with
                  | Not_found -> s
                in
                options :=
                  (va, s, Html.has_attribute tag "selected") :: !options))
          (fun _ -> mach#pop_action)
      and close_select (fo : Htmlfmt.formatter) =
        mach#remove_tag "option";
        let fr =
          fo.create_embedded (Html.get_attribute !tselect "align") None None
        in
        fm.select fr (List.rev !options) !tselect
      in

      mach#add_tag "select" open_select close_select;

      (* 8.1.4 Text Area: TEXTAREA *)
      let textarea_initial = Ebuffer.create 128
      and ttextarea = ref Html.{ tag_name = "textarea"; attributes = [] } in
      let open_textarea _fo tag =
        ttextarea := tag;
        Ebuffer.reset textarea_initial;
        mach#push_action (fun s -> Ebuffer.output_string textarea_initial s)
      and close_textarea (fo : Htmlfmt.formatter) =
        mach#pop_action;
        let _nameTODO = Html.get_attribute !ttextarea "name" in
        let fr =
          fo.create_embedded (Html.get_attribute !ttextarea "align") None None
        in
        fm.textarea fr (Ebuffer.get textarea_initial) !ttextarea
      in

      mach#add_tag "textarea" open_textarea close_textarea)
    (fun _fo -> [ "input"; "select"; "textarea" ] |> List.iter mach#remove_tag)

(*end *)
(*e: display/html_form.ml *)

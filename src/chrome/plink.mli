(*s: gui/plink.mli *)
(*s: signature [[Plink.make]] *)
val make : Hyper.link -> Www.request
    (* [make hlink] is an error correcting version of Www.make
       For invalid links, a dialog box is displayed and offers
       edition facilities
     *)
(*e: signature [[Plink.make]] *)
(*e: gui/plink.mli *)

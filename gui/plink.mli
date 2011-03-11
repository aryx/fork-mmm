val make : Hyper.link -> Www.request
    (* [make hlink] is an error correcting version of Www.make
       For invalid links, a dialog box is displayed and offers
       edition facilities
     *)


let add_ref = ref (fun _ _ -> failwith "no Timer_.add defined")

let set_ref = ref (fun _ _ -> failwith "no Timer_.set defined")

let add (a:int) (b: unit -> unit) : unit =
  !add_ref a b

let set (a:int) (b: unit -> unit) : unit =
  !set_ref a b


type msg = {
  dest    : string;
  subject : string;
  body    : string 
}

val sendmail: msg -> unit

val f: Www.request -> unit


val mailer: string ref
val internal_backend: (string -> string -> unit) ref


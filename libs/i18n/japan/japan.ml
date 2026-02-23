let debug = ref false

type code =
    ISO8859
  | UTF8
  | Binary
  | Unknown

let encode_table =
  [ "utf[-_]?8", UTF8;
    "iso[-_]?8859[-_]?.*", ISO8859;
    "ascii", ISO8859 ]

type detected_code =
    Code of code
  | Mixed

let detected_code_names = [
  Code ISO8859, "iso-8859";
  Code UTF8, "utf-8";
  Code Binary, "binary...";
  Code Unknown, "unknown...";
  Mixed, "mixed coded"
]

type read_type = bytes -> int -> int -> int

class read_native (read_func : read_type) =
 object
  val native_read = read_func
  method read = native_read
end

class read_i18n (read_func) =
 object
  inherit read_native (read_func)
  method set_code = fun (_code : code) -> ()
  method get_code = Code Unknown
end

let input_from_string s =
  let cur = ref 0 in
  fun buf start len ->
    let r =
      try
        String.blit s !cur buf start len;
        len
      with Invalid_argument _ ->
        let r = String.length s - !cur in
        String.blit s !cur buf start r; r
    in
    cur := !cur + r;
    r

type japanese_config = {
    mutable code : code option;
  }

let default_config () = {
  code = None;
}

let change_to_iso8859 c = c.code <- Some ISO8859
let change_to_utf8 c = c.code <- Some UTF8
let change_to_autodetect c = c.code <- None

let create_read_native read_func = new read_i18n read_func
let create_read_japanese read_func _config = new read_i18n read_func

let encoder _code = fun s -> s

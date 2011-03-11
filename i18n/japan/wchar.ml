open Charset

(* Note JISX0208 .... are not expressed with Ku-ten code *)
type wchar = charset * string (* its length is charset dependent *)
type wstring = wchar array

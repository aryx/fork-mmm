(lang dune 3.7)
; use >3.7 otherwise 2.9 will generate error about bin broken symlink
(name mmm)
(version 0.1.0)

(package
 (name mmm)
 (synopsis "A simple Web browser")
 (description "A _really_ long description of a simple project")
 (source (github aryx/fork-mmm))
 (authors "Francois Rouaix")
 (maintainers "Yoann Padioleau <yoann.padioleau@gmail.com>")
 (depends
  (ocaml (>= 4.14.0))
  ppx_deriving
  camlp-streams
  conf-tk
  ssl
  ;NO: (labltk), we use the vendored more basic camltk in external/
))

;(package
; (name ocamltk)
; (synopsis "XXX")
; (description "A _really_ long description of a simple project")
; (source (github aryx/fork-mmm))
; (authors "Yoann Padioleau <yoann.padioleauo@gmail.com>")
; (maintainers "Yoann Padioleau <yoann.padioleau@gmail.com>")
; (depends
;  (ocaml (>= 4.14.0))
;  conf-tk
;))

(generate_opam_files)

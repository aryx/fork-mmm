## CHECK: This is the Makefile.camltk that was installed by ocamltk.
include /usr/local/lib/ocaml/camltk/Makefile.camltk

## CHECK: installation directories
# The first one contains resource files, doc, and the actual binaries for mmm
INSTALLLIBDIR=/usr/local/lib/mmm
# The second one contains the mmm launch script and other binaries
INSTALLBINDIR=/usr/local/bin
MANDIR=/usr/man/man1
MANEXT=1

## Common link directives for contributed libraries
# CHECK: The unix library might require additional C libraries
WITH_UNIX=unix.cma -cclib -lunix
WITH_UNIX_OPT=unix.cmxa -cclib -lunix
WITH_STR=str.cma -cclib -lstr
WITH_STR_OPT=str.cmxa -cclib -lstr
# These should be platform independent
WITH_DYNLINK=dynlink.cma

### Shouldn't need to change anything below
### Tools from the Objective Caml distribution
### LIBDIR is defined by Makefile.camltk
EXCRC=$(LIBDIR)/extract_crc
EXPUNGE=$(LIBDIR)/expunge

CAMLC=ocamlc
CAMLCPP=ocamlc -pp camlp4o
CAMLOPT=ocamlopt
CAMLOPTPP=ocamlopt -pp camlp4o
CAMLYACC=ocamlyacc -v
CAMLLEX=ocamllex
CAMLLIBR=$(CAMLC) -a
CAMLDEP=ocamldep
CAMLDEPPP=ocamldep -pp camlp4o
COMPFLAGS=
LINKFLAGS=

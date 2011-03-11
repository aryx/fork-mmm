# Generated automatically from Makefile.config.in by configure.
## Where you installed ObjectiveCaml
LIBDIR=/usr/local/lib/objective-caml

## Where you want to install CamlTk libraries and binaries
INSTALLDIR=$(LIBDIR)/camltk

### What to use to compile and link with X
X11_LIBS=-ccopt " -L/usr/X11R6/lib" -cclib " -lSM -lICE -lX11 "
X_CFLAGS=-ccopt " -I/usr/X11R6/include"

### What to use to compile and link with Tk
TCLTKINCLUDES=-ccopt "-I/usr/local/lib/tcl7.6/include -I /usr/local/lib/tk4.2/include"
TKLIBS=-ccopt "-L/usr/local/lib" -cclib "-ltk4.2 -ltcl7.6 -ldl -lm"

### Making a library
RANLIB=ranlib

### Shouldn't need to change anything below
## Tools from the Objective Caml distribution
EXCRC=$(LIBDIR)/extract_crc
EXPUNGE=$(LIBDIR)/expunge

CAMLC=ocamlc
CAMLCOMP=$(CAMLC) -c
CPP=/lib/cpp -P -Dunix
CAMLYACC=ocamlyacc -v
CAMLLEX=ocamllex
CAMLLIBR=$(CAMLC) -a
CAMLDEP=ocamldep
COMPFLAGS=
LINKFLAGS=

## End of configuration section

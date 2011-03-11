# Generated automatically from Makefile.camltk.in by configure.
## Where you installed ObjectiveCaml
LIBDIR=/usr/local/lib/objective-caml

## Where installed CamlTk libraries and binaries
CAMLTKDIR=$(LIBDIR)/camltk

TKCOMPFLAGS=-I $(CAMLTKDIR)
WITH_TK=-ccopt -L$(CAMLTKDIR) -cclib -lcamltk \
	-ccopt "-L/usr/local/lib" -cclib "-ltk4.2 -ltcl7.6 -ldl -lm" \
	-ccopt " -L/usr/X11R6/lib" -cclib " -lSM -lICE -lX11 " \
	$(TKCOMPFLAGS) camltk.cma
WITH_FRX=libfrx.cma
WITH_JPF=libjpf.cma
WITH_JTK=$(CAMLTKDIR)/jtk.cmo
WITH_TKANIM=tkanim.cmo tkaniminit.cmo $(CAMLTKDIR)/tkaniminit.o -cclib -ltkanim

### Usage:
### in your Makefiles, add the following
##  include .../Makefile.camltk
### Then add $(TKCOMPFLAGS) to COMPFLAGS
## COMPFLAGS=$(TKCOMPFLAGS)
### and the rule to produce a standlone CamlTk application is
## appl : $(OBJS)
##	ocamlc -custom -o $@ $(WITH_TK) $(OBJS)

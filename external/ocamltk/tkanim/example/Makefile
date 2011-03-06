include ../../Makefile.config

COMPFLAGS=-I ../../support -I ../../lib -I ..
LINKFLAGS= -I ../../lib

all : gifanimtest

OBJS = gifanimtest.cmo

TKLINKOPT= -ccopt -L../../support -cclib -lcamltk41 \
	   -ccopt -L../ext \
	   $(TKLIBS) $(X11_LIBS) \
	   tk41.cma unix.cma -cclib -lunix 

gifanimtest: $(OBJS)
	$(CAMLC) -custom $(LINKFLAGS) $(TKLINKOPT) -o gifanimtest \
		../tkanim.cmo ../tkaniminit.o ../tkaniminit.cmo $(OBJS) \
		-cclib -ltkanim
			

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

depend: 
	$(CAMLDEP) *.mli *.ml > .depend

clean:
	rm -f gifanimtest *.cm?

include .depend

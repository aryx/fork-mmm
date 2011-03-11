include ../../Makefile.config

COMPFLAGS=-I ../../support -I ../../lib -I ..
LINKFLAGS= -I ../../lib

EXECS=gifanimtest$(EXE)

all : $(EXECS)

OBJS = gifanimtest.cmo

TKLINKOPT= -ccopt -L../../support -cclib -lcamltk \
	   -ccopt -L../ext \
	   ../cltkaniminit.o -cclib -ltkanim \
	   $(TKLIBS) $(X11_LIBS) \
	   camltk.cma unix.cma -cclib -lunix 

gifanimtest$(EXE): $(OBJS)
	$(CAMLC) -custom $(LINKFLAGS) $(TKLINKOPT) -o gifanimtest$(EXE) \
		../tkanim.cmo $(OBJS)


.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

depend: 
	$(CAMLDEP) *.mli *.ml > .depend

clean:
	rm -f $(EXECS) *.cm?

include .depend

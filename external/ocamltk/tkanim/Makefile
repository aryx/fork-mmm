include ../Makefile.config

COMPFLAGS=-I ../support -I ../lib

all: tkanim.cmo cltkaniminit.o tkaniminit.cmo
	cd ext; $(MAKE)

opt: tkanim.cmx cltkaniminit.o tkaniminit.cmx

cltkaniminit.o: cltkaniminit.c
	$(CAMLC) -c $(X_CFLAGS) $(TCLTKINCLUDES) cltkaniminit.c 

clean:
	rm -f *.cm* *.o
	cd ext; $(MAKE) clean 
	cd example; $(MAKE) clean

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp .cmx

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<


install: tkanim.cmo cltkaniminit.o tkaniminit.cmo
	cp tkanim.cm* tkaniminit.cm* cltkaniminit.o $(INSTALLDIR)
	cd ext; $(MAKE) install

installopt: tkanim.o tkanim.cmx tkaniminit.o tkaniminit.cmx
	cp tkanim.cmx tkanim.o tkaniminit.cmx tkaniminit.o $(INSTALLDIR)

depend: tkanim.ml
	$(CAMLDEP) *.mli *.ml > .depend

include .depend

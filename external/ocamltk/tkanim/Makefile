include ../Makefile.config

COMPFLAGS=-I ../support -I ../lib

all: tkanim.cmo cltkaniminit.o tkaniminit.cmi
	cd ext; $(MAKE)

opt: tkanim.cmx cltkaniminit.o tkaniminit.cmi

cltkaniminit.o: cltkaniminit.c
	$(CAMLC) -c $(X_CFLAGS) $(COMPFLAGS) $(TCLTKINCLUDES) cltkaniminit.c 

clean:
	rm -f *.cm* *.o
	cd ext; $(MAKE) clean 
	cd example; $(MAKE) clean

distclean: clean
	-rm -f ext/Makefile

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp .cmx

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<


install: tkanim.cmo cltkaniminit.o tkaniminit.cmi
	cp tkanim.cm* tkaniminit.cmi *.mli cltkaniminit.o $(INSTALLDIR)
	cd ext; $(MAKE) install

installopt: tkanim.o tkanim.cmx 
	cp tkanim.cmx tkanim.o cltkaniminit.o $(INSTALLDIR)

depend: tkanim.ml
	$(CAMLDEP) *.mli *.ml > .depend

include .depend

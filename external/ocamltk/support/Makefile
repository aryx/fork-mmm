include ../Makefile.config

all: support.cmo widget.cmo protocol.cmo \
     textvariable.cmo timer.cmo fileevent.cmo \
     libcamltk.a

opt: support.cmx widget.cmx protocol.cmx \
     textvariable.cmx timer.cmx fileevent.cmx \
     libcamltk.a

# Experimental support for dynamically loading Caml into Wish
shared: libcamltks.a 


COBJS=cltkCaml.o cltkEval.o cltkFile.o cltkMain.o cltkEvent.o \
      cltkMisc.o cltkTimer.o cltkVar.o cltkWait.o cltkImg.o

SOBJS=$(COBJS:.o=.s.o)

CCFLAGS=-ccopt -g $(X_CFLAGS) $(TCLTKINCLUDES)

# -fPIC is platform dependent
CCSFLAGS=-ccopt -fPIC -ccopt -g $(X_CFLAGS) $(TCLTKINCLUDES)

libcamltk.a : $(COBJS)
	rm -f libcamltk.a
	ar rc libcamltk.a $(COBJS)

libcamltks.a: $(SOBJS)
	ar rc $@ $(SOBJS)


PUB=fileevent.cmi fileevent.mli \
    protocol.cmi protocol.mli \
    textvariable.cmi textvariable.mli \
    timer.cmi timer.mli \
    widget.cmi widget.mli 

install: libcamltk.a $(PUB)
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(PUB) $(INSTALLDIR)
	cp libcamltk.a $(INSTALLDIR)
	cd $(INSTALLDIR); chmod 644 $(PUB) libcamltk.a
	$(RANLIB) $(INSTALLDIR)/libcamltk.a

clean : 
	rm -f *.cm* *.o *.a

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .cmx .mlp .c .o .s.o

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.c.o:
	$(CAMLCOMP) $(CCFLAGS) $<

.c.s.o:
	@ if test -f $*.o; then mv $*.o $*.f.o; else :; fi
	$(CAMLCOMP) $(CCSFLAGS) $<
	mv $*.o $*.s.o
	@ if test -f $*.f.o; then mv $*.f.o $*.o; else :; fi

depend:
	$(CAMLDEP) *.mli *.ml > .depend

include .depend

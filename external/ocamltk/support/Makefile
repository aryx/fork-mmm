include ../Makefile.config

all: support.cmo widget.cmo protocol.cmo \
     textvariable.cmo timer.cmo fileevent.cmo \
     libcamltk41.a

opt: support.cmx widget.cmx protocol.cmx \
     textvariable.cmx timer.cmx fileevent.cmx \
     libcamltk41.a

# Experimental support for dynamically loading Caml into Wish
shared: libcamltk41s.a 


COBJS=cltkCaml.o cltkEval.o cltkFile.o cltkMain.o cltkEvent.o \
      cltkMisc.o cltkTimer.o cltkVar.o cltkWait.o cltkImg.o

SOBJS=$(COBJS:.o=.s.o)

CCFLAGS=-ccopt -g $(X_CFLAGS) $(TCLTKINCLUDES)

# -fPIC is platform dependent
CCSFLAGS=-ccopt -fPIC -ccopt -g $(X_CFLAGS) $(TCLTKINCLUDES)

libcamltk41.a : $(COBJS)
	rm -f libcamltk41.a
	ar rc libcamltk41.a $(COBJS)

libcamltk41s.a: $(SOBJS)
	ar rc $@ $(SOBJS)


PUB=fileevent.cmi fileevent.mli \
    protocol.cmi protocol.mli \
    textvariable.cmi textvariable.mli \
    timer.cmi timer.mli \
    widget.cmi widget.mli 

install: libcamltk41.a $(PUB)
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(PUB) $(INSTALLDIR)
	cp libcamltk41.a $(INSTALLDIR)
	cd $(INSTALLDIR); chmod 644 $(PUB) libcamltk41.a
	$(RANLIB) $(INSTALLDIR)/libcamltk41.a

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

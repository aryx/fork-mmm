include ../Makefile.config

COMPFLAGS= -I ../support

TKLINKOPT=$(STATIC) \
          -ccopt -L../support -cclib -lcamltk41 \
	  $(TKLIBS) $(X11_LIBS)

LIBROPT= -cclib -lcamltk41 $(TKLIBS) $(X11_LIBS)

SUPPORT=../support/support.cmo ../support/widget.cmo ../support/protocol.cmo \
        ../support/textvariable.cmo ../support/timer.cmo \
        ../support/fileevent.cmo

SUPPORTX = $(SUPPORT:.cmo=.cmx)

all : tk41.cma ocamltktop
      
opt : tk41.cmxa

include ./modules

WIDGETOBJSX = $(WIDGETOBJS:.cmo=.cmx)

tk41.cma : $(SUPPORT) $(WIDGETOBJS) tk.cmo 
	$(CAMLLIBR) -o tk41.cma $(SUPPORT) tk.cmo $(WIDGETOBJS) $(LIBROPT)

tk41.cmxa : $(SUPPORTX) $(WIDGETOBJSX) tk.cmx
	$(CAMLOPTLIBR) -o tk41.cmxa $(SUPPORTX) tk.cmx $(WIDGETOBJSX) $(LIBROPT)

## Until ocamltktop is fixed (next release), we in-line it
## (otherwise our trick with -ccopt is broken)

ocamltktop : $(WIDGETOBJS) $(SUPPORT) ../support/libcamltk41.a
	$(CAMLC) -custom -linkall -o ocamltktop -I ../support $(TKLINKOPT) \
	       toplevellib.cma tk41.cma topmain.cmo

# All .{ml,mli} files are generated in this directory
clean : 
	rm -f *.cm* *.ml *.mli *.o *.a tk41.cmo  ocamltktop

install: tk41.cma ocamltktop
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp *.cmi *.mli tk41.cma ocamltktop $(INSTALLDIR)
	chmod 644 $(INSTALLDIR)/*.cmi
	chmod 644 $(INSTALLDIR)/*.mli
	chmod 644 $(INSTALLDIR)/tk41.cma
	chmod 755 $(INSTALLDIR)/ocamltktop

installopt: tk41.cmxa
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp tk41.cmxa tk41.a $(INSTALLDIR)
	chmod 644 $(INSTALLDIR)/tk41.cmxa
	chmod 644 $(INSTALLDIR)/tk41.a


.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .cmx .mlp

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<
  

include .depend

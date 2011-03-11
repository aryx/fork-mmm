include ../Makefile.config

COMPFLAGS= -I ../support

TKLINKOPT=$(STATIC) \
          -ccopt -L../support -cclib -lcamltk \
	  $(TKLIBS) $(X11_LIBS)

LIBROPT= -cclib -lcamltk $(TKLIBS) $(X11_LIBS)

SUPPORT=../support/support.cmo ../support/widget.cmo ../support/protocol.cmo \
        ../support/textvariable.cmo ../support/timer.cmo \
        ../support/fileevent.cmo

SUPPORTX = $(SUPPORT:.cmo=.cmx)

all : camltk.cma
# ocamltktop$(EXE)

opt : camltk.cmxa

include ./modules

WIDGETOBJSX = $(WIDGETOBJS:.cmo=.cmx)

camltk.cma : $(SUPPORT) $(WIDGETOBJS) tk.cmo 
	$(CAMLLIBR) -o camltk.cma $(SUPPORT) tk.cmo $(WIDGETOBJS) $(LIBROPT)

camltk.cmxa : $(SUPPORTX) $(WIDGETOBJSX) tk.cmx
	$(CAMLOPTLIBR) -o camltk.cmxa $(SUPPORTX) tk.cmx $(WIDGETOBJSX) $(LIBROPT)

## Until ocamltktop is fixed (next release), we in-line it
## (otherwise our trick with -ccopt is broken)

ocamltktop$(EXE) : $(WIDGETOBJS) $(SUPPORT) ../support/libcamltk.a
	$(CAMLC) -custom -linkall -o ocamltktop$(EXE) -I ../support \
	   $(TKLINKOPT) toplevellib.cma camltk.cma topmain.cmo

# All .{ml,mli} files are generated in this directory
clean : 
	rm -f *.cm* *.ml *.mli *.o *.a camltk.cmo ocamltktop$(EXE) .depend

# ocamltktop$(EXE)
install: camltk.cma
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp *.cmi *.mli camltk.cma  $(INSTALLDIR)
	rm -f $(INSTALLDIR)/tkgen.ml $(INSTALLDIR)/tkgen.mli # they are temporary files.
	chmod 644 $(INSTALLDIR)/*.cmi
	chmod 644 $(INSTALLDIR)/*.mli
	chmod 644 $(INSTALLDIR)/camltk.cma
#	chmod 755 $(INSTALLDIR)/ocamltktop$(EXE)
# cp ocamltktop$(EXE) $(INSTALLDIR)

installopt: camltk.cmxa
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp camltk.cmxa camltk.a $(INSTALLDIR)
	chmod 644 $(INSTALLDIR)/camltk.cmxa
	chmod 644 $(INSTALLDIR)/camltk.a


.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .cmx .mlp

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	ocamldep -slash -I ../support/ *.mli *.ml > .depend

-include .depend


include ../Makefile.config

COMPFLAGS=-I ../lib -I ../support

OBJS= frx_misc.cmo frx_widget.cmo frx_font.cmo frx_entry.cmo frx_text.cmo \
      frx_listbox.cmo frx_req.cmo frx_fillbox.cmo frx_focus.cmo \
      frx_dialog.cmo frx_mem.cmo frx_rpc.cmo frx_synth.cmo frx_selection.cmo \
      frx_after.cmo frx_fit.cmo frx_ctext.cmo frx_color.cmo

OBJSX = $(OBJS:.cmo=.cmx)

all: libfrx.cma

opt: libfrx.cmxa

libfrx.cma: $(OBJS)
	$(CAMLLIBR) -o libfrx.cma $(OBJS)

libfrx.cmxa: $(OBJSX)
	$(CAMLOPTLIBR) -o libfrx.cmxa $(OBJSX)
 

install: libfrx.cma
	cp *.cmi *.mli libfrx.cma $(INSTALLDIR)

installopt: libfrx.cmxa
	cp libfrx.cmxa libfrx.a $(INSTALLDIR)
 

clean:
	rm -f *.cm* *.o *.a

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<


depend: 
	$(CAMLDEP) *.mli *.ml > .depend

include .depend

TOP=../..
include $(TOP)/Makefile.config

INCLUDES=-I $(TOP) -I $(TOP)/commons -I $(TOP)/www -I $(TOP)/html 

DEPFLAGS=$(INCLUDES)

COMPFLAGS= $(TKCOMPFLAGS) $(INCLUDES)

all : surfboard surfboard_remote

JDIR=$(TOP)/i18n/japan
JAPAN= 	$(JDIR)/bug.cmo \
	$(JDIR)/charset.cmo $(JDIR)/wchar.cmo $(JDIR)/wstream.cmo \
        $(JDIR)/tool.cmo \
	$(JDIR)/jisx0201.cmo \
	$(JDIR)/lexkanji.cmo \
	$(JDIR)/encode.cmo \
	$(JDIR)/japan.cmo

LIBS=$(TOP)/commons/lib.cma

HTML=$(TOP)/html/dtd.cmo $(TOP)/html/html.cmo $(TOP)/html/lexhtml.cmo $(TOP)/html/html_eval.cmo

OBJS= misc.cmo \
      tree.cmo \
      connection.cmo \
      version_sboard.cmo \
      treeview.cmo \
      addpage.cmo \
      main_sboard.cmo

surfboard: $(OBJS)
	$(CAMLC) -custom -ccopt "-L/opt/X11/lib" -o surfboard \
		unix.cma str.cma $(WITH_TK) $(WITH_JTK) $(WITH_FRX) \
		$(LIBS) $(JAPAN) \
		$(TOP)/www/uri.cmo $(HTML) $(OBJS)

#		 $(WITH_UNIX) $(WITH_STR) \
#		 $(WITH_TK) $(WITH_JTK) $(WITH_FRX) \
#		 $(MMMOBJS) $(OBJS)

#surfboardx: $(OBJS:.cmo=.cmx)
#	 $(CAMLOPT) -o surfboardx \
#		 $(WITH_UNIX_OPT) $(WITH_STR_OPT) \
#		 $(WITH_TK_OPT) $(WITH_JTK_OPT) $(WITH_FRX_OPT) \
#		 $(MMMOBJS:.cmo=.cmx) $(OBJS:.cmo=.cmx)

surfboard_remote: surfboard_remote.cmo
	$(CAMLC) -custom -o surfboard_remote \
		unix.cma surfboard_remote.cmo

install: surfboard
	echo "$(MAKE) install for surfboard: not yet"

clean::
	rm -f *.cm?
	rm -f surfboard
	rm -f surfboard_remote

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

depend: 
	$(CAMLDEP) $(INCLUDES) *.ml .mli > .depend

-include .depend

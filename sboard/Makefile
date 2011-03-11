include ../Makefile.config

INCLUDES=-I .. -I ../misc -I ../www -I ../html 

DEPFLAGS=$(INCLUDES)

COMPFLAGS= $(TKCOMPFLAGS) $(INCLUDES)

all : surfboard surfboard_remote

JDIR=../i18n/japan
JAPAN= 	$(JDIR)/bug.cmo \
	$(JDIR)/charset.cmo $(JDIR)/wchar.cmo $(JDIR)/wstream.cmo \
        $(JDIR)/tool.cmo \
	$(JDIR)/jisx0201.cmo \
	$(JDIR)/lexkanji.cmo \
	$(JDIR)/encode.cmo \
	$(JDIR)/japan.cmo

MISC=../misc/log.cmo ../misc/low.cmo ../misc/mstring.cmo ../misc/mlist.cmo ../misc/msys.cmo \
     ../misc/ebuffer.cmo ../misc/munix.cmo \
     ../misc/date.cmo \
     ../misc/i18nprintf.cmo ../misc/i18n.cmo \
     ../misc/error.cmo

HTML=../html/dtd.cmo ../html/html.cmo ../html/lexhtml.cmo ../html/html_eval.cmo

OBJS= misc.cmo \
      tree.cmo \
      connection.cmo \
      version.cmo \
      treeview.cmo \
      addpage.cmo \
      main.cmo

surfboard: $(OBJS)
	$(CAMLC) -custom -ccopt "-L/opt/local/lib" -o surfboard \
		../lang.cmo $(WITH_UNIX) $(WITH_STR) $(WITH_TK) $(WITH_JTK) $(WITH_FRX) \
		$(MISC) $(JAPAN) \
		../www/uri.cmo $(HTML) $(OBJS)

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
		$(WITH_UNIX) surfboard_remote.cmo

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

include .depend

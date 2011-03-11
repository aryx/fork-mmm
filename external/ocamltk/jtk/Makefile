include ../Makefile.config

COMPFLAGS=-I ../support -I ../lib

all : jtk.cmo # $(WIDGETOBJS) are compiled into jtk.cmo

opt : jtk.cmx

include ./modules

jtk.ml jtk.mli: Jtk.src builtin.ml builtin.mli
	../compiler/tkcompiler -outdir . Jtk.src
	(\
	echo "open Protocol"; \
	echo "open Tk"; \
	echo "open Widget"; \
	cat tkgen.ml; \
	cat builtin.ml; \
	for i in $(WIDGETOBJS) ; do\
	 m=`basename $$i .cmo`;\
	 mm=`../safe/capitalize $$m`;\
	 echo "module $$mm = struct"; \
	 sed -e '/^open/d' $$m.ml; \
         echo "end"; \
	 echo; \
        done \
	) > jtk.ml
	sed -n -e '/^(\* type \*)$$/,/^(\* \/type \*)$$/p' tkgen.ml | \
	sed -e '/^(\* type \*)$$/d' -e '/^(\* \/type \*)$$/d' \
         > tktypes.ml
	(\
	echo "open Protocol"; \
	echo "open Tk"; \
	echo "open Widget"; \
	cat tktypes.ml; \
	cat builtin.mli; \
	for i in $(WIDGETOBJS) ; do\
	 m=`basename $$i .cmo`;\
	 mm=`../safe/capitalize $$m`;\
	 echo "module $$mm : sig"; \
	 sed -e '/^open/d' $$m.mli; \
         echo "end"; \
	 echo; \
        done \
	) > jtk.mli

clean:
	rm -f *.cm? jtk.ml jtk.mli tkgen.ml tkgen.mli kanji.m* kanji.cm* tktypes.ml lib *.o

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp .cmx

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

install: jtk.cmo
	cp jtk.cm* jtk.mli $(INSTALLDIR)

installopt: jtk.cmx jtk.o
	cp jtk.cmx jtk.o $(INSTALLDIR)

depend:
	$(CAMLDEP) *.mli *.ml > .depend

include .depend

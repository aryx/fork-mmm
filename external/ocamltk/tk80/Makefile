include ../Makefile.config

COMPFLAGS=-I ../support -I ../lib

all : tk80.cmo # $(WIDGETOBJS) are compiled into tk80.cmo

opt : tk80.cmx

include ./modules

tk80.ml tk80.mli: Tk80.src
	../compiler/tkcompiler -outdir . Tk80.src
	(\
	echo "open Protocol"; \
	echo "open Tk"; \
	echo "open Widget"; \
	cat builtin.ml; \
	cat tkgen.ml; \
	for i in $(WIDGETOBJS) ; do\
	 m=`basename $$i .cmo`;\
	 mm=`../safe/capitalize $$m`;\
	 echo "module $$mm = struct"; \
	 sed -e '/^open/d' $$m.ml; \
         echo "end"; \
	 echo; \
        done \
	) > tk80.ml
	sed -n -e '/^(\* type \*)$$/,/^(\* \/type \*)$$/p' tkgen.ml | \
	sed -e '/^(\* type \*)$$/d' -e '/^(\* \/type \*)$$/d' \
         > tktypes.ml
	(\
	echo "open Protocol"; \
	echo "open Tk"; \
	echo "open Widget"; \
	cat builtin.mli; \
	cat tktypes.ml; \
	for i in $(WIDGETOBJS) ; do\
	 m=`basename $$i .cmo`;\
	 mm=`../safe/capitalize $$m`;\
	 echo "module $$mm : sig"; \
	 sed -e '/^open/d' $$m.mli; \
         echo "end"; \
	 echo; \
        done \
	) > tk80.mli

clean:
	rm -f *.cm? tk80.ml tk80.mli tkgen.ml tkgen.mli tk80.cm* tktypes.ml lib *.o 

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp .cmx

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

install: tk80.cmo
	cp tk80.mli tk80.cm* $(INSTALLDIR)

installopt: tk80.cmx tk80.o
	cp tk80.cmx tk80.o $(INSTALLDIR)

depend:
	$(CAMLDEP) *.mli *.ml > .depend

include .depend

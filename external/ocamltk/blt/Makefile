include ../Makefile.config
COMPFLAGS=-I ../support -I ../lib
CCFLAGS=-ccopt -g $(X_CFLAGS) $(TCLTKINCLUDES)

all : blt.cma libcamlblt.a

opt : blt.cmxa

include modules

OBJS=blt.cmo
OBJSX = $(OBJS:.cmo=.cmx)

blt.cma: $(OBJS)
	$(CAMLC) -a -o $@ $(OBJS)

blt.cmxa: $(OBJSX)
	$(CAMLOPT) -a -o $@ $(OBJSX)

COBJS=bltinit.o

libcamlblt.a: $(COBJS)
	rm -f $@
	ar rc $@ $(COBJS)

install: blt.cma libcamlblt.a
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp blt.cmi blt.mli blt.cma libcamlblt.a $(INSTALLDIR)

installopt: blt.cmxa libcamlblt.a
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp blt.cmi blt.mli blt.cmxa blt.a libcamlblt.a $(INSTALLDIR)

blt.ml blt.mli: blt.src builtin.ml builtin.mli
	../compiler/tkcompiler$(EXE) -outdir . blt.src
	(\
	echo "open Tk"; \
	echo "open Widget"; \
	echo "open Support"; \
	echo "open Protocol"; \
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
	) > blt.ml
	sed -n -e '/^(\* type \*)$$/,/^(\* \/type \*)$$/p' tkgen.ml | \
	sed -e '/^(\* type \*)$$/d' -e '/^(\* \/type \*)$$/d' \
         > tktypes.ml
	(\
	echo "open Tk"; \
	echo "open Widget"; \
	echo "open Support"; \
	echo "open Protocol"; \
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
	) > blt.mli

clean:
	rm -f *.cm*
	rm -f *.o *.a
	rm -f blt.ml* tkgen.ml* dragDrop.ml* table.ml* tktypes.ml

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .cmx .mlp .c .o

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<
  
.c.o:
	$(CAMLCOMP) $(CCFLAGS) $<

blt.cmo: blt.cmi


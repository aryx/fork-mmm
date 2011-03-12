CAMLCOMP=ocamlc -c -nopervasives
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc
CAMLLIBR=ocamlc -a
CAMLDEP=ocamldep


COMPFLAGS= -I ../sandbox/gen

OBJS=images.cmo tachy_test.cmo tachy_space.cmo audio.cmo \
	tachy_aftermmm.cmo remove_simple_table.cmo

all: $(OBJS)

clean:
	rm -f core *.o *.cm* *~


.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

depend:
	$(CAMLDEP) *.ml *.mli > .depend

include .depend

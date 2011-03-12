CAMLCOMP=ocamlc -c -nopervasives
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc
CAMLLIBR=ocamlc -a
CAMLDEP=ocamldep

TOP=../..

COMPFLAGS= -I $(TOP)/sandbox/gen

OBJS=foo.cmo cube3d.cmo molecule.cmo \
     mondrian.cmo taquin.cmo \
     goto.cmo slide.cmo icons.cmo \
     morph.cma \
     eyes.cmo dancing.cmo japanese.cmo rotimage.cmo tetris.cmo

all: $(OBJS)

morph.cma: morpher.cmo mmorph.cmo
	$(CAMLLIBR) -o $@ morpher.cmo mmorph.cmo

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

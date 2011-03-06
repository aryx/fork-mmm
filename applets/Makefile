CAMLCOMP=ocamlc -c -nopervasives
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc
CAMLLIBR=ocamlc -a
CAMLDEP=ocamldep


COMPFLAGS= -I ../safe

OBJS=cube3d.cmo molecule.cmo \
     mondrian.cmo taquin.cmo \
     goto.cmo slide.cmo icons.cmo \
     morph.cma \
     eyes.cmo dancing.cmo japanese.cmo rotimage.cmo tetris.cmo

all: $(OBJS)

morph.cma: morpher.cmo mmorph.cmo
	$(CAMLLIBR) -o $@ morpher.cmo mmorph.cmo

clean:
	rm -f core *.o *.cm* *~

cube3d.ml:
	-ln -s ../shared/applets/$@ $@
molecule.ml:
	-ln -s ../shared/applets/$@ $@
mondrian.ml:
	-ln -s ../shared/applets/$@ $@
taquin.ml:
	-ln -s ../shared/applets/$@ $@
goto.ml:
	-ln -s ../shared/applets/$@ $@
slide.ml:
	-ln -s ../shared/applets/$@ $@
icons.ml:
	-ln -s ../shared/applets/$@ $@
morpher.ml:
	-ln -s ../shared/applets/$@ $@
mmorph.ml:
	-ln -s ../shared/applets/$@ $@
eyes.ml:
	-ln -s ../shared/applets/$@ $@
dancing.ml:
	-ln -s ../shared/applets/$@ $@
japanese.ml:
	-ln -s ../shared/applets/$@ $@
rotimage.ml:
	-ln -s ../shared/applets/$@ $@
tetris.ml:
	-ln -s ../shared/applets/$@ $@

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

depend:
	$(CAMLDEP) *.ml *.mli > .depend

include .depend

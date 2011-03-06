include ../../Makefile.camltk

WITH_BLT=blt.cma -cclib -lcamlblt -cclib -lBLT
CAMLCOMP=ocamlc -c

all: blttest

COMPFLAGS=-I .. $(TKCOMPFLAGS)
LINKFLAGS=-I .. -ccopt -L..
OBJS=main.cmo


blttest: $(OBJS)
	ocamlc -custom -o $@ $(LINKFLAGS) $(WITH_TK) $(WITH_BLT) $(OBJS)

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp .c .o

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.c.o:
	$(CAMLCOMP) $(CCFLAGS) $<


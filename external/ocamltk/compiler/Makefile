include ../Makefile.config

OBJS=tsort.cmo tables.cmo lexer.cmo parser.cmo  compile.cmo intf.cmo \
     maincompile.cmo

tkcompiler : $(OBJS)
	$(CAMLC) $(LINKFLAGS) -o tkcompiler $(OBJS)

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

parser.ml parser.mli: parser.mly
	$(CAMLYACC) -v parser.mly

clean : 
	rm -f *.cm*  parser.ml parser.mli lexer.ml tkcompiler parser.output

scratch :
	rm -f *.cm*  parser.ml parser.mli lexer.ml tkcompiler

install: 
	cp tkcompiler $(INSTALLDIR)

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

depend: parser.ml parser.mli lexer.ml
	$(CAMLDEP) *.mli *.ml > .depend

include .depend

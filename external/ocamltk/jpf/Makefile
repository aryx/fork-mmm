include ../Makefile.config

COMPFLAGS=-I ../lib -I ../support

OBJS= shell.cmo fileselect.cmo balloon.cmo jpf_font.cmo
OBJSX = $(OBJS:.cmo=.cmx)

TKLINKOPT=$(STATIC) \
	  -ccopt -L../support -cclib -lcamltk41 \
	  $(TKLIBS) $(X11_LIBS)

all: libjpf.cma

opt: libjpf.cmxa

test: balloontest

testopt: balloontest.opt

libjpf.cma: $(OBJS)
	$(CAMLLIBR) -o libjpf.cma $(OBJS)

libjpf.cmxa: $(OBJSX)
	$(CAMLOPTLIBR) -o libjpf.cmxa $(OBJSX)

install: libjpf.cma
	cp *.cmi *.mli libjpf.cma $(INSTALLDIR)

installopt: libjpf.cmxa
	cp libjpf.cmxa libjpf.a $(INSTALLDIR)

clean:
	rm -f *.cm* *~ *test

### Tests

balloontest: balloontest.cmo 
	$(CAMLC) -o balloontest -I ../support -I ../lib \
	        -custom tk41.cma libjpf.cma balloontest.cmo $(TKLINKOPT)

balloontest.cmo : balloon.cmo libjpf.cma

balloontest.opt: balloontest.cmx
	$(CAMLOPT) -o balloontest.opt -I ../support -I ../lib \
	        tk41.cmxa libjpf.cmxa balloontest.cmx $(TKLINKOPT)

balloontest.cmx : balloon.cmx libjpf.cmxa


.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<


depend: 
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
	 $(CAMLDEP) *.mli *.ml) > Makefile


### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
balloon.cmo: balloon.cmi 
balloon.cmx: balloon.cmi 
balloontest.cmo: balloon.cmi 
balloontest.cmx: balloon.cmx 
fileselect.cmo: shell.cmi fileselect.cmi 
fileselect.cmx: shell.cmx fileselect.cmi 
jpf_font.cmo: shell.cmi jpf_font.cmi 
jpf_font.cmx: shell.cmx jpf_font.cmi 
shell.cmo: shell.cmi 
shell.cmx: shell.cmi 

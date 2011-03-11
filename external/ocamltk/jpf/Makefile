include ../Makefile.config

COMPFLAGS=-I ../lib -I ../support

OBJS= shell.cmo fileselect.cmo balloon.cmo jpf_font.cmo
OBJSX = $(OBJS:.cmo=.cmx)

TKLINKOPT=$(STATIC) \
	  -ccopt -L../support -cclib -lcamltk \
	  $(TKLIBS) $(X11_LIBS)

all: libjpf.cma

opt: libjpf.cmxa

test: balloontest$(EXE)

testopt: balloontest_opt$(EXE)

libjpf.cma: $(OBJS)
	$(CAMLLIBR) -o libjpf.cma $(OBJS)

libjpf.cmxa: $(OBJSX)
	$(CAMLOPTLIBR) -o libjpf.cmxa $(OBJSX)

install: libjpf.cma
	cp *.cmi *.mli libjpf.cma $(INSTALLDIR)

installopt: libjpf.cmxa
	cp libjpf.cmxa libjpf.a $(INSTALLDIR)

clean:
	rm -f *.cm* *~ *.o *.a
	rm -f balloontest$(EXE) balloontest_opt$(EXE)

### Tests

balloontest$(EXE): balloontest.cmo 
	$(CAMLC) -o balloontest$(EXE) -I ../support -I ../lib \
	        -custom camltk.cma libjpf.cma balloontest.cmo $(TKLINKOPT)

balloontest.cmo : balloon.cmo libjpf.cma

balloontest_opt$(EXE): balloontest.cmx
	$(CAMLOPT) -o balloontest_opt$(EXE) -I ../support -I ../lib \
	        camltk.cmxa libjpf.cmxa balloontest.cmx $(TKLINKOPT)

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

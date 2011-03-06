include ../Makefile.config

# We are using the non-installed library !
COMPFLAGS= -I ../lib -I ../support
LINKFLAGS= -I ../lib

# Use pieces of Makefile.config
TKLINKOPT=-ccopt -L../support -cclib -lcamltk41 \
	  $(TKLIBS) $(X11_LIBS) \
          tk41.cma

all: addition helloworld winskel fileinput

addition: addition.cmo
	$(CAMLC) -custom $(LINKFLAGS) $(TKLINKOPT) \
		-o $@ addition.cmo 

helloworld: helloworld.cmo
	$(CAMLC) -custom $(LINKFLAGS) $(TKLINKOPT) \
		-o $@ helloworld.cmo 

winskel: winskel.cmo
	$(CAMLC) -custom $(LINKFLAGS) $(TKLINKOPT) \
		-o $@ winskel.cmo

fileinput: fileinput.cmo
	$(CAMLC) -custom $(LINKFLAGS) unix.cma $(TKLINKOPT) -cclib -lunix \
		-o $@ fileinput.cmo

socketinput: socketinput.cmo
	$(CAMLC) -custom $(LINKFLAGS) unix.cma $(TKLINKOPT) -cclib -lunix \
		-o $@ socketinput.cmo

clean : 
	rm -f *.cm? addition helloworld winskel fileinput

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

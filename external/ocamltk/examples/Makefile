include ../Makefile.config

# We are using the non-installed library !
COMPFLAGS= -I ../lib -I ../support
LINKFLAGS= -I ../lib

# Use pieces of Makefile.config
#TKLINKOPT=-ccopt -L../support -cclib -lcamltk \
	  $(TKLIBS) $(X11_LIBS) \
          camltk.cma
TKLINKOPT=-I +camltk camltk.cma

EXECS=addition$(EXE) helloworld$(EXE) winskel$(EXE) fileinput$(EXE) \
eyes$(EXE) tetris$(EXE) mytext$(EXE) fileopen$(EXE)

all: $(EXECS)

addition$(EXE): addition.cmo
	$(CAMLC) -custom $(LINKFLAGS) $(TKLINKOPT) \
		-o $@ addition.cmo 

helloworld$(EXE): helloworld.cmo
	$(CAMLC) -custom $(LINKFLAGS) $(TKLINKOPT) \
		-o $@ helloworld.cmo 

winskel$(EXE): winskel.cmo
	$(CAMLC) -custom $(LINKFLAGS) $(TKLINKOPT) \
		-o $@ winskel.cmo

fileinput$(EXE): fileinput.cmo
	$(CAMLC) -custom $(LINKFLAGS) unix.cma $(TKLINKOPT) -cclib -lunix \
		-o $@ fileinput.cmo

socketinput$(EXE): socketinput.cmo
	$(CAMLC) -custom $(LINKFLAGS) unix.cma $(TKLINKOPT) -cclib -lunix \
		-o $@ socketinput.cmo

eyes$(EXE): eyes.cmo
	$(CAMLC) -custom $(LINKFLAGS) unix.cma $(TKLINKOPT) \
		-o $@ eyes.cmo

tetris$(EXE): tetris.cmo
	$(CAMLC) -custom $(LINKFLAGS) unix.cma $(TKLINKOPT) \
		-o $@ tetris.cmo

mytext$(EXE): mytext.cmo
	$(CAMLC) -custom $(LINKFLAGS) unix.cma $(TKLINKOPT) \
		-o $@ mytext.cmo

graph$(EXE): graphics.cmo graphics_test.cmo
	$(CAMLC) -custom $(LINKFLAGS) $(TKLINKOPT) graphics.cmo \
		-o $@ graphics_test.cmo

graphics_test.cmo: graphics.cmo

fileopen$(EXE): fileopen.cmo
	$(CAMLC) -custom $(LINKFLAGS) $(TKLINKOPT) -o $@ fileopen.cmo

clean : 
	rm -f *.cm? $(EXECS) a.out

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

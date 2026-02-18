TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= tkresource.ml\
  tk_condition.ml\
  tk_error.ml\
  tk_i18n.ml\
  tk_document.ml\
  tk_auth.ml\
  tk_file.ml\
  tk_mailto.ml\
  tk_progress.ml\
  glevents.ml\
  img.ml

SYSLIBS= str.cma unix.cma 
LIBS=
INCLUDEDIRS=\
 $(TOP)/libs/commons\
 $(TOP)/libs/misc\
 $(TOP)/src/url\
 $(TOP)/src/www\
 $(TOP)/src/http\
 $(TOP)/src/protocols\
 $(TOP)/src/retrieve\


SYSINCLUDES=$(TKCOMPFLAGS)

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top


#old: for tk ressource and its use of the parser extension
# PP=-pp camlp4o
#but I switched to raw Stream API without needing the OCaml stream
# extension

tkresource.cmo: tkresource.cmi
	$(OCAMLC) $(PP) -c tkresource.ml

tkresource.cmx: tkresource.cmi
	$(OCAMLOPT) $(PP) -c tkresource.ml

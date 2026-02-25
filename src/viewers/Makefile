TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= decoders.ml save.ml viewers.ml plain.ml embed.ml 

SYSLIBS= str.cma unix.cma 
LIBS=

INCLUDEDIRS=$(TOP)/libs/commons $(TOP)/libs/misc $(TOP)/libs/i18n \
  $(TOP)/src/core $(TOP)/src/url $(TOP)/src/www $(TOP)/src/http \
  $(TOP)/src/protocols $(TOP)/src/retrieve \
  $(TOP)/src/tk

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

##############################################################################
# Literate Programming rules
##############################################################################

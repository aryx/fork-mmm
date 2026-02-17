TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= decoders.ml save.ml viewers.ml plain.ml embed.ml 

SYSLIBS= str.cma unix.cma 
LIBS=

INCLUDEDIRS=$(TOP)/commons $(TOP)/globals \
  $(TOP)/url $(TOP)/www $(TOP)/http $(TOP)/protocols \
  $(TOP)/retrieve \
  $(TOP)/tk

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

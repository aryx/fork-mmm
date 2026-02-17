TOP=../../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

#INCLUDES+=$(TKCOMPFLAGS)

SRC= bug.ml \
     charset.ml wchar.ml wstream.ml \
     tool.ml \
     jisx0201.ml \
     lexkanji.ml \
     encode.ml \
     japan.ml

SYSLIBS= str.cma unix.cma 
LIBS=

INCLUDEDIRS=$(TOP)/libs/commons

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


lexkanji.ml : lexkanji.mll
	$(CAMLLEX) lexkanji.mll
clean::
	rm -f lexkanji.ml
beforedepend:: lexkanji.ml

##############################################################################
# Literate Programming rules
##############################################################################

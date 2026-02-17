TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= base64.ml lexdate.ml http_headers.ml \
     lexheaders.ml auth.ml \
     http.ml retype.ml \
     http_date.ml

SYSLIBS= str.cma unix.cma 
LIBS=

INCLUDEDIRS=$(TOP)/libs/commons $(TOP)/url $(TOP)/www $(TOP)/globals
SYSINCLUDES=

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


lexheaders.ml : lexheaders.mll
	$(CAMLLEX) lexheaders.mll
lexdate.ml : lexdate.mll
	$(CAMLLEX) lexdate.mll
clean::
	rm -f lexheaders.ml lexdate.ml
beforedepend:: lexheaders.ml lexdate.ml

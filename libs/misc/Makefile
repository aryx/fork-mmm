TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

# Miscellaneous additional libraries
SRC= mstring.ml mlist.ml msys.ml \
     ebuffer.ml munix.ml date.ml log.ml ibtree.ml


SYSLIBS= str.cma unix.cma 
LIBS=
INCLUDEDIRS=$(TOP)/libs/commons
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


##############################################################################
# Literate Programming rules
##############################################################################

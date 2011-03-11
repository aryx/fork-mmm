TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

OCAMLCFLAGS+=$(TKCOMPFLAGS)

# Miscellaneous additional libraries
SRC=low.cmo mstring.cmo mlist.cmo msys.cmo \
     ebuffer.cmo munix.cmo date.cmo \
     log.cmo \
     condition.cmo feed.cmo \
     lexpath.cmo ibtree.cmo \
     tkresource.cmo glevents.cmo \
     i18nprintf.cmo lang.cmo i18n.cmo \
     error.cmo hotlist.cmo

SYSLIBS= str.cma unix.cma 
LIBS=

INCLUDEDIRS=

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


# for tk ressource and its use of the parser extension
PP=-pp camlp4o

tkresource.cmo: tkresource.cmi
	$(OCAMLC) $(PP) -c tkresource.ml

tkresource.cmx: tkresource.cmi
	$(OCAMLOPT) $(PP) -c tkresource.ml


lexpath.ml : lexpath.mll
	$(CAMLLEX) lexpath.mll
clean::
	rm -f lexpath.ml
beforedepend:: lexpath.ml

##############################################################################
# Literate Programming rules
##############################################################################

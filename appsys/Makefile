TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= pgp.ml capabilities.ml dload.ml applets.ml appview.ml

SYSLIBS= str.cma unix.cma 
LIBS=

INCLUDEDIRS=$(TOP)/commons $(TOP)/globals \
 $(TOP)/www $(TOP)/http \
 $(TOP)/protocols $(TOP)/viewers

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


# Most files are shared with the Calves plugin
applets.ml:
	-ln -s ../shared/$@ $@
applets.mli:
	-ln -s ../shared/$@ $@
capabilities.ml:
	-ln -s ../shared/$@ $@
capabilities.mli:
	-ln -s ../shared/$@ $@
dload.ml:
	-ln -s ../shared/$@ $@
dload.mli:
	-ln -s ../shared/$@ $@
pgp.ml:
	-ln -s ../shared/$@ $@

beforedepend:: applets.ml applets.mli \
	       capabilities.ml capabilities.mli \
	       dload.ml dload.mli \
	       pgp.ml

##############################################################################
# Literate Programming rules
##############################################################################

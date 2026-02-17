TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= about.ml gcache.ml \
     fontprefs.ml prefs.ml mmmprefs.ml \
     history.ml plink.ml nav.ml \
     mmm.ml cci.ml \
     debug.ml

SYSLIBS= str.cma unix.cma 
LIBS=

INCLUDEDIRS=$(TOP)/libs/commons $(TOP)/globals $(TOP)/libs/i18n/japan \
 $(TOP)/url $(TOP)/www $(TOP)/http \
 $(TOP)/languages/html $(TOP)/protocols \
 $(TOP)/retrieve \
 $(TOP)/tk \
 $(TOP)/viewers \
 $(TOP)/display \

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

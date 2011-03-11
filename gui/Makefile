TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= about.cmo gcache.cmo \
     fontprefs.cmo prefs.cmo mmmprefs.cmo \
     history.cmo plink.cmo nav.cmo \
     mmm.cmo cci.cmo \
     debug.cmo 

SYSLIBS= str.cma unix.cma 
LIBS=

INCLUDEDIRS=$(TOP)/commons $(TOP)/globals $(TOP)/i18n/japan \
 $(TOP)/www $(TOP)/http \
 $(TOP)/html $(TOP)/protocols \
 $(TOP)/retrieve $(TOP)/viewers \
 $(TOP)/display

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

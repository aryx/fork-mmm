TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= \
 fonts.ml htbind.ml attrs.ml \
 cmap.ml fit.ml \
 hr.ml source.ml ctext.ml \
 imgload.ml \
 form.ml html_form.ml \
 table.ml html_table.ml \
 styles.ml \
 htframe.ml \
 textw_fo.ml html_disp.ml htmlw.ml

#  html_form.ml html_table.ml\
#       hr.ml form.ml fit.ml table.ml \
#       fonts.ml styles.ml htbind.ml \
#       cmap.ml imgload.ml html_disp.ml \
#       source.ml attrs.ml \
#       ctext.ml cmap.ml textw_fo.ml  \
#       htframe.ml htmlw.ml

## Extra dependencies (functor application) that ocamldep doesn't find
#htmlw.cmo: textw_fo.cmi imgload.cmi \
#	 	form.cmi table.cmi
#htmlw.cmx: textw_fo.cmx imgload.cmx \
#	 	form.cmx table.cmx

SYSLIBS= str.cma unix.cma 
LIBS=

INCLUDEDIRS=$(TOP)/libs/commons $(TOP)/libs/misc \
 $(TOP)/libs/i18n \
 $(TOP)/src/core $(TOP)/src/url $(TOP)/src/www $(TOP)/src/http \
 $(TOP)/languages/html \
 $(TOP)/src/retrieve $(TOP)/src/protocols \
 $(TOP)/src/viewers $(TOP)/src/tk

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

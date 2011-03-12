TOP=..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SYSLIBS= str.cma unix.cma 
LIBS=

INCLUDEDIRS= \
 $(TOP)/commons \
 $(TOP)/www $(TOP)/http $(TOP)/html \
 $(TOP)/retrieve $(TOP)/protocols $(TOP)/viewers \
 $(TOP)/applets $(TOP)/display $(TOP)/gui \
 gen

SYSINCLUDES=$(TKCOMPFLAGS)

#pad: the rm gen/*.ml is really just for codemap ...
all:: gen/safe418.cmi gen/safe418.cmo gen/safe418mmm.cmi gen/safe418mmm.cmo crcs.cmo crcsmmm.cmo
	rm gen/*.ml*

gen/safe418mmm.mli: gen/safe418.mli safemmm.mli
	(echo '(* Automatically generated. Do NOT edit *)'; \
	 cat $^ ) > $@

gen/safe418mmm.ml: gen/safe418.ml safemmm.ml
	(echo '(* Automatically generated. Do NOT edit *)'; \
	 cat $^ ) > $@

safe418mmm.cmi: gen/safe418mmm.mli $(APPSYS) $(OBJS)
	$(CAMLC) $(SYSINCLUDES) -c -nopervasives gen/safe418mmm.mli

safe418mmm.cmo: safe418mmm.cmi \
	                    gen/safe418mmm.ml $(APPSYS) $(OBJS)
	$(CAMLC) $(SYSINCLUDES) $(INCLUDES) $(COMPFLAGS) -c gen/safe418mmm.ml

crcsmmm.ml: safe418mmm.cmi
	-mv $@ $@.bak
	$(LIBDIR)/extract_crc -I gen -I $(LIBDIR) \
	  Safe418mmm \
	  > $@ || rm $@
	@-diff $@ $@.bak || echo "WARNING: CRCs have changed"

beforedepend:: gen/safe418mmm.ml gen/safe418mmm.mli

clean::
	rm -f safe418mmm.cm*
	rm -f gen/safe418mmm.ml gen/safe418mmm.mli

# just for authors
safe-precheck:
	md5 $(CAMLTKDIR)/*.cmi | sort -n | md5

safe: safe-common safe-mmm crcs.ml crcsmmm.ml

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

# This file should be included in applications wishing to support
# the "common" applets (e.g. MMM, plug-ins).
#
# Safe library for applets
#
# We must build a single file for all safe libraries. It is not possible
# anymore to build a "tower" of safe libraries because type equality 
# constraints needed for that purpose will force internal signatures to
# be exported (this is a fix to ensure type-safe linking !)

## For applets common to MMM and Calves
# The interfaces
SAFEINTF=safestd.mli \
        safetk.mli safefrx.mli safejtk.mli safetkanim.mli \
	safeapplets.mli safeio.mli safestr.mli safeunix.mli

# The implementations
SAFEIMPL=$(SAFEINTF:.mli=.ml)

# Note the version number
safe-common: safe418.cmi safe418.cmo

gen/safe418.mli: $(SAFEINTF)
	(echo '(* Automatically generated. Do NOT edit *)'; \
	cat $(SAFEINTF)) > $@

gen/safe418.ml: $(SAFEIMPL)
	(echo '(* Automatically generated. Do NOT edit *)'; \
	cat $(SAFEIMPL)) > $@

safe418.cmi: gen/safe418.mli $(APPSYS) $(OBJS)
	$(CAMLC) $(SYSINCLUDES) -c -nopervasives gen/safe418.mli

safe418.cmo: safe418.cmi gen/safe418.ml \
	                 $(APPSYS) $(OBJS)
	$(CAMLC) $(SYSINCLUDES) $(COMPFLAGS) -c gen/safe418.ml

beforedepend:: $(SAFEINTF) $(SAFEIMPL)

# safeapplet.ml depends on the particular implementation

# These come from ocamltk
safetk.ml:
	-ln -s $(CAMLTKDIR)/safetk.ml $@
safetk.mli:
	-ln -s $(CAMLTKDIR)/safetk.mli $@
safefrx.ml:
	-ln -s $(CAMLTKDIR)/safefrx.ml $@
safefrx.mli:
	-ln -s $(CAMLTKDIR)/safefrx.mli $@
safetkanim.ml:
	-ln -s $(CAMLTKDIR)/safetkanim.ml $@
safetkanim.mli:
	-ln -s $(CAMLTKDIR)/safetkanim.mli $@
safejtk.ml:
	-ln -s $(CAMLTKDIR)/safejtk.ml $@
safejtk.mli:
	-ln -s $(CAMLTKDIR)/safejtk.mli $@

crcs.ml : safe418.cmi
	-mv $@ $@.bak
	$(LIBDIR)/extract_crc -I gen -I $(LIBDIR) \
	  Oo Safe418 \
	  > $@ || rm $@
	@-diff $@ $@.bak || echo "WARNING: CRCs have changed"

clean:: clean-safe

clean-safe:
	rm -f safe418.cm*
	rm -f gen/*
	rm -f safetk.ml safetk.mli \
	      safefrx.ml safefrx.mli \
	      safetkanim.ml safetkanim.mli \
	      safejtk.ml safejtk.mli	

depend::
	$(OCAMLDEP) -I gen *.mli *.ml gen/*.ml* > .depend

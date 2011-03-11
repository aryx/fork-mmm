#############################################################################
# Configuration section
#############################################################################
include Makefile.config

##############################################################################
# Variables
##############################################################################
TOP=$(shell pwd)

SRC=main.ml

TARGET=lib

SYSLIBS=

MAKESUBDIRS= commons i18n/japan globals \
  www http html protocols retrieve viewers \

INCLUDEDIRS=$(MAKESUBDIRS) htdisp appsys browser safe

LIBS=$(MAKESUBDIRS:%=%/lib.cma)

# This number defines the revision of the applet system
VERSION=418

#------------------------------------------------------------------------------
# Program related variables
#------------------------------------------------------------------------------

PROGS=mmm

#PROGS+=htparse, surboard

OPTPROGS= $(PROGS:=.opt)

#------------------------------------------------------------------------------
#package dependencies
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Main variables
#------------------------------------------------------------------------------
COMPFLAGS=$(TKCOMPFLAGS) $(INCLUDES)
OCAMLCFLAGS+=$(COMPFLAGS)

##############################################################################
# Generic
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################

.PHONY: all all.opt opt top clean distclean   applets modules

# Bytecode targets
all:: 
	$(MAKE) rec 
	$(MAKE) allbyte 

opt:
	$(MAKE) rec.opt 
	$(MAKE) $(TARGET).opt

allbyte: mmm.bin mmm_remote htparse surfboard applets modules

all.opt: opt
top: $(TARGET).top

rec:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done 

rec.opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt || exit 1; done 


# OBJS are common for all versions
OBJS= $(LIBS) $(HTDISP) $(TXTDISP) $(BROWSER)

# Entry point
MAIN=main.cmo

clean::
	rm -f main.cm*

# Exported safe libraries
SAFE= appsys/appsys.cmo safe/safe$(VERSION).cmo safe/safe$(VERSION)mmm.cmo
CRCS= safe/crcs.cmo safe/crcsmmm.cmo 
mmm.bin: $(OBJS) $(CRCS) $(APPSYS) $(SAFE) $(MAIN)
	$(CAMLC) -custom -ccopt "-L/opt/local/lib" -o $@ $(LINKFLAGS) \
		$(WITH_DYNLINK) $(WITH_UNIX) $(WITH_STR) $(WITH_TK) \
	        $(WITH_FRX) $(WITH_JPF) $(WITH_TKANIM) $(WITH_JTK) \
		$(WITH_TK80) \
		$(OBJS) $(CRCS) $(APPSYS) $(SAFE) $(MAIN)

# The native version does not support applets !
mmmx.bin: $(OBJS:.cmo=.cmx) $(MAIN:.cmo=.cmx) 
	$(CAMLOPT) -o $@ $(PROFILING) \
	  $(WITH_UNIX_OPT) $(WITH_STR_OPT) $(WITH_TK_OPT) \
	  $(WITH_FRX_OPT) $(WITH_JPF_OPT) $(WITH_TKANIM_OPT) $(WITH_JTK_OPT) \
	  $(WITH_TK80_OPT) \
	  $(OBJS:.cmo=.cmx) $(MAIN:.cmo=.cmx)


# The standalone HTML syntax checker
HTMISC=commons/ebuffer.cmo commons/log.cmo

HTML=html/dtd.cmo html/html.cmo html/lexhtml.cmo html/html_eval.cmo

htparse: commons/lang.cmo $(HTMISC) i18n/japan/lib.cma $(HTML) html/htparse.cmo
	$(CAMLC) $(LINKFLAGS) -o $@ $^

# Remote command
mmm_remote: remote/mmm_remote.cmo
	$(CAMLC) -custom -o mmm_remote $(WITH_UNIX) remote/mmm_remote.cmo

# JPF's bookmark tool
surfboard: 
	cd sboard; $(MAKE)

clean::
	cd sboard; $(MAKE) clean

# Applets examples
applets:
	cd applets; $(MAKE) all

clean::
	cd applets; $(MAKE) clean

# User's additional modules examples
modules:
	cd modules; $(MAKE) all

clean::
	cd modules; $(MAKE) clean

clean::
	rm -rf htdisp/*.cm* htdisp/*.o
	rm -rf appsys/*.cm* appsys/*.o
	rm -rf safe/*.cm* safe/*.o
	rm -rf browser/*.cm* browser/*.o
	rm -rf remote/*.cm* remote/*.o
	rm -f mmm.bin mmmx.bin mmm_remote htparse

# Default rules

depend:: beforedepend
	(for d in commons; \
	do $(CAMLDEPPP) $(INCLUDES) $$d/*.mli $$d/*.ml; \
	done; \
        for d in www http html protocols retrieve viewers htdisp appsys browser i18n/japan safe; \
	do $(CAMLDEP) $(INCLUDES) $$d/*.mli $$d/*.ml; \
	done; $(CAMLDEP) main.ml* ) > .depend
	cd sboard; $(MAKE) depend

include .depend


# add -custom so dont need add e.g. ocamlbdb/ in LD_LIBRARY_PATH
CUSTOM=-custom

static:
	rm -f $(EXEC).opt $(EXEC)
	$(MAKE) STATIC="-ccopt -static" $(EXEC).opt
	cp $(EXEC).opt $(EXEC)

purebytecode:
	rm -f $(EXEC).opt $(EXEC)
	$(MAKE) BYTECODE_STATIC="" $(EXEC)


distclean:: clean
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done
	rm -f Makefile.config

##############################################################################
# Build documentation
##############################################################################

##############################################################################
# Misc rules
##############################################################################

# Browser (communication protocols, multimedia viewer dispatch, ...)

# HTML viewer per se
HTDISP=htdisp/html_form.cmo htdisp/html_table.cmo\
       htdisp/hr.cmo htdisp/form.cmo htdisp/fit.cmo htdisp/table.cmo \
       htdisp/fonts.cmo htdisp/styles.cmo htdisp/htbind.cmo \
       htdisp/cmap.cmo htdisp/imgload.cmo htdisp/html_disp.cmo \
       htdisp/source.cmo htdisp/attrs.cmo \
       htdisp/ctext.cmo htdisp/textw_fo.cmo \
       htdisp/htframe.cmo htdisp/htmlw.cmo


# Extra dependencies (functor application) that ocamldep doesn't find
htdisp/htmlw.cmo: htdisp/textw_fo.cmi htdisp/imgload.cmi \
	 	htdisp/form.cmi htdisp/table.cmi
htdisp/htmlw.cmx: htdisp/textw_fo.cmx htdisp/imgload.cmx \
	 	htdisp/form.cmx htdisp/table.cmx

# Plain text viewer
TXTDISP=viewers/plain.cmo

# Browser GUI, prefs, navigation logic
BROWSER=browser/about.cmo browser/gcache.cmo \
	browser/fontprefs.cmo browser/prefs.cmo browser/mmmprefs.cmo \
        browser/history.cmo browser/plink.cmo browser/nav.cmo \
	browser/mmm.cmo browser/cci.cmo \
        browser/debug.cmo 

# The applet system
APPSYS=appsys/pgp.cmo appsys/capabilities.cmo appsys/dload.cmo \
       appsys/applets.cmo appsys/appview.cmo

# Most files are shared with the Calves plugin
appsys/applets.ml:
	-ln -s ../shared/$@ $@
appsys/applets.mli:
	-ln -s ../shared/$@ $@
appsys/capabilities.ml:
	-ln -s ../shared/$@ $@
appsys/capabilities.mli:
	-ln -s ../shared/$@ $@
appsys/dload.ml:
	-ln -s ../shared/$@ $@
appsys/dload.mli:
	-ln -s ../shared/$@ $@
appsys/pgp.ml:
	-ln -s ../shared/$@ $@

beforedepend:: appsys/applets.ml appsys/applets.mli \
	       appsys/capabilities.ml appsys/capabilities.mli \
	       appsys/dload.ml appsys/dload.mli \
	       appsys/pgp.ml

appsys: $(APPSYS)

## The common safe library
include Makefile.safe

## For applets specific to MMM
SAFEMMMINTF= safe/safe$(VERSION).mli safe/safemmm.mli
SAFEMMMIMPL= safe/safe$(VERSION).ml safe/safemmm.ml

safe-mmm: $(SAFEMMM) safe/safe$(VERSION)mmm.cmi safe/safe$(VERSION)mmm.cmo

SAFEMMM= $(SAFEMMMINTF) $(SAFEMMMIMPL)

safe/safe$(VERSION)mmm.mli: $(SAFEMMMINTF)
	(echo '(* Automatically generated. Do NOT edit *)'; \
	 cat $(SAFEMMMINTF)) > $@

safe/safe$(VERSION)mmm.ml: $(SAFEMMMIMPL)
	(echo '(* Automatically generated. Do NOT edit *)'; \
	 cat $(SAFEMMMIMPL)) > $@

safe/safe$(VERSION)mmm.cmi: safe/safe$(VERSION)mmm.mli $(APPSYS) $(OBJS)
	$(CAMLC) -c -nopervasives safe/safe$(VERSION)mmm.mli

safe/safe$(VERSION)mmm.cmo: safe/safe$(VERSION)mmm.cmi \
	                    safe/safe$(VERSION)mmm.ml $(APPSYS) $(OBJS)
	$(CAMLC) $(COMPFLAGS) -c safe/safe$(VERSION)mmm.ml

safe/crcsmmm.ml: safe/safe$(VERSION)mmm.cmi
	-mv $@ $@.bak
	$(LIBDIR)/extract_crc -I safe -I $(LIBDIR) \
	  Safe$(VERSION)mmm \
	  > $@ || rm $@
	@-diff $@ $@.bak || echo "WARNING: CRCs have changed"

beforedepend:: safe/safe$(VERSION)mmm.ml safe/safe$(VERSION)mmm.mli

clean::
	rm -f safe/safe$(VERSION)mmm.cm*
	rm -f safe/safe$(VERSION)mmm.ml safe/safe$(VERSION)mmm.mli

# just for authors
safe-precheck:
	md5 $(CAMLTKDIR)/*.cmi | sort -n | md5

safe: safe-common safe-mmm safe/crcs.ml safe/crcsmmm.ml



##############################################################################
# Install
##############################################################################

## Installation : copy the various files and binaries
## Preprocess shell-script
install: mmm.bin htparse mmm_remote mmm.sh install-mdk
	if [ ! -d $(INSTALLBINDIR) ]; then \
          mkdir -p $(INSTALLBINDIR); \
        fi
	if [ ! -d $(INSTALLLIBDIR) ]; then \
          mkdir -p $(INSTALLLIBDIR); \
        fi 
	cp htparse mmm_remote $(INSTALLBINDIR)
	/bin/rm -rf $(INSTALLLIBDIR)/*
	cp mmm.bin $(INSTALLLIBDIR)
	cp msgs*.txt $(INSTALLLIBDIR)
	cp MMM.ad MMM.ad.* $(INSTALLLIBDIR)
	cp -pr doc $(INSTALLLIBDIR)	
	sed -e '/_INSTALLLIBDIR_/s,_INSTALLLIBDIR_,$(INSTALLLIBDIR),g' \
	     mmm.sh > $(INSTALLBINDIR)/mmm
	chmod 755 $(INSTALLBINDIR)/mmm
	for i in doc/*.m; do cp $$i $(MANDIR)/`basename $$i .m`.$(MANEXT); done


## Post-install configuration (?)
setup:
	@echo -n "Default proxy host name []: "; \
	read proxy_host; \
	echo -n "Default proxy host port [80]: "; \
        read proxy_port; \
	(echo "g/^proxy=/s/.*/proxy=$$proxy_host/p"; \
	 echo "g/^port=/s/.*/port=$$proxy_port/p";   \
	 echo w; \
	 echo q) | ed $(INSTALLBINDIR)/mmm

## If we build the native version
installopt: mmmx.bin
	if [ ! -d $(INSTALLLIBDIR) ]; then \
         mkdir -p $(INSTALLLIBDIR); \
        fi
	cp mmmx.bin $(INSTALLLIBDIR)

install-mdk:
	if [ ! -d $(INSTALLLIBDIR)/mdk ]; then \
         mkdir -p $(INSTALLLIBDIR)/mdk; \
        fi
	cp safe/safe$(VERSION)*.mli safe/safe$(VERSION)*.cmi \
	    $(INSTALLLIBDIR)/mdk
	sed -e '/_MDKDIR_/s,_MDKDIR_,$(INSTALLLIBDIR)/mdk,g' \
	     scripts/mmmc > $(INSTALLBINDIR)/mmmc
	chmod 755 $(INSTALLBINDIR)/mmmc

##############################################################################
# Package rules
##############################################################################

# Just for MMM maintainers, distribution of the software
FTPDIR=/net/pauillac/infosystems/ftp/cristal/mmm

distribute:
	rm -rf release
	mkdir release
	cd release; cvs co mmm/ocaml; \
	find . -name '.cvsignore' -print | xargs rm; \
	find . -name 'CVS' -print | xargs rm -rf; \
	mv mmm/ocaml mmm$(VERSION); \
	tar cvf mmm$(VERSION)-src.tar mmm$(VERSION); \
	gzip mmm$(VERSION)-src.tar; \
	mv -f mmm$(VERSION)-src.tar.gz $(FTPDIR)
	chgrp caml $(FTPDIR)/mmm$(VERSION)-src.tar.gz 	
	rm -rf release
	cd doc; $(MAKE) install

##############################################################################
# Developer rules
##############################################################################

DIRS=$(filter-out commons2, $(MAKESUBDIRS))

PP1=-pp camlp4o
# you want "-dot-reduce"
# don't put "-dot-colors white"; using colors ocamldoc generates one
#  color per directory ! quite useful
# todo? generate a graph using the  -dot-types flag ? (type dependencies)
dotall:
	ocamldoc $(PP1) $(TKCOMPFLAGS) $(INCLUDES) $(DIRS:=/*.ml) -dot -dot-reduce 
	perl -p -i -e 's/\[style=filled, color=white\]//;' ocamldoc.out
	dot -Tps ocamldoc.out > dot.ps
	mv dot.ps Fig_graph_ml.ps
	ps2pdf Fig_graph_ml.ps
	rm -f Fig_graph_ml.ps

tags:
	~/pfff/stags -verbose -lang ml .

##############################################################################
# Pad specific rules
##############################################################################

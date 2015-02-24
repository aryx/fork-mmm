#############################################################################
# Configuration section
#############################################################################
include Makefile.config

##############################################################################
# Variables
##############################################################################
TOP=$(shell pwd)

PROGS=mmm
#PROGS+=htparse, surfboard

OPTPROGS= $(PROGS:=.opt)

SRC=main.ml

# for the basic browser
MAINDIRS= \
  commons i18n/japan globals \
  www http html \
  protocols retrieve viewers \
  display \
  gui \
  applets \
  sandbox \

# dynamically loaded extensions
MOREDIRS= extensions demos/applets demos/sboard

MAKESUBDIRS= $(MAINDIRS) $(MOREDIRS)

INCLUDEDIRS=$(MAINDIRS) sandbox/gen crcs
LIBS=$(MAINDIRS:%=%/lib.cma)

# use dynlink for the applet system
SYSLIBS=unix.cma str.cma  dynlink.cma
TKLIBS=$(WITH_TK) $(WITH_FRX) $(WITH_JPF) $(WITH_TKANIM) $(WITH_JTK)\
  $(WITH_TK80)

SYSINCLUDES=$(TKCOMPFLAGS)

##############################################################################
# Generic
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################

.PHONY: all all.opt opt top clean distclean

# Bytecode targets
all:: 
	$(MAKE) rec 
	$(MAKE) allbyte 

opt:
	$(MAKE) rec.opt 
	$(MAKE) $(TARGET).opt

allbyte: mmm mmm_remote htparse

all.opt: opt
top: $(TARGET).top

rec:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done 

rec.opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt || exit 1; done 


# OBJS are common for all versions
OBJS= $(LIBS) $(TXTDISP)

# Entry point
MAIN=main.cmo

clean::
	rm -f main.cm*
clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 

depend::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done

# Exported sandbox libraries
SAFE= applets/appsys.cmo sandbox/gen/safe418.cmo sandbox/gen/safe418mmm.cmo
CRCS= crcs/crcs.cmo crcs/crcsmmm.cmo 
mmm: $(OBJS) $(CRCS) $(SAFE) $(MAIN)
	$(OCAMLC) -custom -ccopt "-L/opt/X11/lib" -o $@ $(LINKFLAGS) \
         $(SYSLIBS) $(TKLIBS) $^

# The native version does not support applets !
mmmx.bin: $(OBJS:.cmo=.cmx) $(MAIN:.cmo=.cmx) 
	$(OCAMLOPT) -o $@ $(PROFILING) \
        $(SYSLIBS:%.cma=%.cmxa)
	  $(WITH_TK_OPT) \
	  $(WITH_FRX_OPT) $(WITH_JPF_OPT) $(WITH_TKANIM_OPT) $(WITH_JTK_OPT) \
	  $(WITH_TK80_OPT) \
	  $(OBJS:.cmo=.cmx) $(MAIN:.cmo=.cmx)

# The standalone HTML syntax checker
HTMISC=commons/lang.cmo commons/ebuffer.cmo commons/log.cmo
htparse: $(HTMISC) i18n/japan/lib.cma html/lib.cma
	$(CAMLC) $(LINKFLAGS) -o $@ $^

# Remote command
mmm_remote: main_remote.cmo
	$(CAMLC) -custom -o $@ unix.cma $^

clean::
	rm -f mmm mmmx.bin mmm_remote htparse

# Default rules
#depend:: beforedepend
#	(for d in commons; \
#	do $(CAMLDEPPP) $(INCLUDES) $$d/*.mli $$d/*.ml; \
#	done; \
#        for d in www http html protocols retrieve viewers display applets gui i18n/japan safe; \
#	do $(CAMLDEP) $(INCLUDES) $$d/*.mli $$d/*.ml; \
#	done; $(CAMLDEP) main.ml* ) > .depend
#	cd sboard; $(MAKE) depend
#include .depend

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
#	rm -f Makefile.config

##############################################################################
# Build documentation
##############################################################################

##############################################################################
# Install
##############################################################################

## Installation : copy the various files and binaries
## Preprocess shell-script
install: mmm htparse mmm_remote mmm.sh install-mdk
	if [ ! -d $(INSTALLBINDIR) ]; then \
          mkdir -p $(INSTALLBINDIR); \
        fi
	if [ ! -d $(INSTALLLIBDIR) ]; then \
          mkdir -p $(INSTALLLIBDIR); \
        fi 
	cp htparse mmm_remote $(INSTALLBINDIR)
	/bin/rm -rf $(INSTALLLIBDIR)/*
	cp mmm $(INSTALLLIBDIR)
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
	cp sandbox/safe418*.mli sandbox/safe418*.cmi \
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

#DIRS=$(filter-out sandbox, $(MAKESUBDIRS))
DIRS=$(filter-out sandbox, $(MAINDIRS))

PP1=-pp camlp4o
# you want "-dot-reduce"
# don't put "-dot-colors white"; using colors ocamldoc generates one
#  color per directory ! quite useful
# todo? generate a graph using the  -dot-types flag ? (type dependencies)

DOTCOLORS=green,darkgoldenrod2,cyan,red,magenta,yellow,burlywood1,aquamarine,purple,lightpink,salmon,mediumturquoise,slategray3,limegreen

dotall:
	ocamldoc $(PP1) $(TKCOMPFLAGS) $(INCLUDES) -I demos/sboard -I demos/applets $(DIRS:=/*.ml) main.ml -dot -dot-reduce -dot-colors $(DOTCOLORS)
	perl -p -i -e 's/\[style=filled, color=white\]//;' ocamldoc.out
	dot -Tps ocamldoc.out > dot.ps
	mv dot.ps Fig_graph_ml.ps
	ps2pdf Fig_graph_ml.ps
	rm -f Fig_graph_ml.ps

tags:
	~/pfff/stags -verbose -lang ml .

graph:
	~/pfff/codegraph -lang cmt -build .

##############################################################################
# Pad specific rules
##############################################################################

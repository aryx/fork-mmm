# All configurable variables should reside in Makefile.config
include Makefile.config

# This number defines the revision of the applet system
VERSION=418

# The list of source subdirectories
INCLUDES=-I japan \
	 -I misc -I www -I http -I html -I protos -I retrieve -I viewers \
         -I htdisp -I appsys -I browser -I safe

# Switch the following two lines to compile MMM with profiling
#PROFILING=-ccopt -p
PROFILING=
DEBUG=-g
# Sum up all compilation flags (except DEBUG, since ocamlopt doesn't grok -g)
COMPFLAGS=$(TKCOMPFLAGS) $(INCLUDES) $(PROFILING)
LINKFLAGS=$(DEBUG)

# Bytecode targets
all : allbyte 

.PHONY: applets modules

allbyte : mmm.bin mmm_remote htparse surfboard applets modules

# Native targets
allopt : opt
opt : mmmx.bin

#
# The sub-systems
#

# Japanese support
JAPAN= 	japan/bug.cmo \
	japan/charset.cmo japan/wchar.cmo japan/wstream.cmo japan/tool.cmo \
	japan/jisx0201.cmo \
	japan/lexkanji.cmo \
	japan/encode.cmo \
	japan/japan.cmo

beforedepend:: japan/lexkanji.ml

japan/lexkanji.ml : japan/lexkanji.mll
	$(CAMLLEX) japan/lexkanji.mll

clean::
	rm -f japan/lexkanji.ml

# Miscellaneous additional libraries
MISC=misc/low.cmo misc/mstring.cmo misc/mlist.cmo misc/msys.cmo \
     misc/ebuffer.cmo misc/munix.cmo misc/date.cmo \
     misc/log.cmo \
     misc/condition.cmo misc/feed.cmo \
     misc/lexpath.cmo misc/ibtree.cmo \
     misc/tkresource.cmo misc/glevents.cmo \
     misc/i18nprintf.cmo misc/i18n.cmo \
     misc/error.cmo misc/hotlist.cmo

beforedepend:: misc/lexpath.ml

misc/lexpath.ml : misc/lexpath.mll
	$(CAMLLEX) misc/lexpath.mll

misc/tkresource.cmo: misc/tkresource.cmi
	$(CAMLCPP) $(COMPFLAGS) $(DEBUG) -c misc/tkresource.ml

misc/tkresource.cmx: misc/tkresource.cmi
	$(CAMLOPTPP) $(COMPFLAGS) -c misc/tkresource.ml

clean::
	rm -f misc/lexpath.ml

# General WWW definitions
WWW=www/url.cmo www/uri.cmo www/urlenc.cmo www/lexurl.cmo www/hyper.cmo \
    www/www.cmo www/document.cmo www/maps.cmo

beforedepend:: www/lexurl.ml

www/lexurl.ml : www/lexurl.mll
	$(CAMLLEX) www/lexurl.mll
clean::
	rm -f www/lexurl.ml


# HTTP library
HTTP=http/base64.cmo http/lexdate.cmo http/http_headers.cmo \
     http/lexheaders.cmo http/auth.cmo \
     http/http.cmo http/retype.cmo \
     http/http_date.cmo

beforedepend:: http/lexheaders.ml http/lexdate.ml

http/lexheaders.ml : http/lexheaders.mll
	$(CAMLLEX) http/lexheaders.mll

http/lexdate.ml : http/lexdate.mll
	$(CAMLLEX) http/lexdate.mll

clean::
	rm -f http/lexheaders.ml http/lexdate.ml

# HTML library
HTML=html/dtd.cmo html/html.cmo html/lexhtml.cmo html/html_eval.cmo

beforedepend:: html/lexhtml.ml

html/lexhtml.ml : html/lexhtml.mll
	$(CAMLLEX) html/lexhtml.mll

clean::
	rm -rf html/lexhtml.ml


# Browser (communication protocols, multimedia viewer dispatch, ...)
PROTOS=protos/file.cmo protos/mailto.cmo protos/cache.cmo protos/protos.cmo
RETRIEVE=retrieve/retrieve.cmo retrieve/scheduler.cmo \
	 retrieve/progress.cmo  retrieve/img.cmo 
VIEWERS=viewers/decoders.cmo viewers/save.cmo \
	viewers/viewers.cmo viewers/embed.cmo 

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

# Entry point
MAIN=browser/main.cmo

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

# OBJS are common for all versions
OBJS= version.cmo lang.cmo \
      $(MISC) $(JAPAN) $(WWW) $(HTTP) $(HTML) $(PROTOS) $(RETRIEVE) \
      $(VIEWERS) $(HTDISP) $(TXTDISP) $(BROWSER)

# Exported safe libraries
SAFE= appsys/appsys.cmo safe/safe$(VERSION).cmo safe/safe$(VERSION)mmm.cmo
CRCS= safe/crcs.cmo safe/crcsmmm.cmo 
mmm.bin: $(OBJS) $(CRCS) $(APPSYS) $(SAFE) $(MAIN)
	$(CAMLC) -custom -o $@ $(LINKFLAGS) \
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
HTMISC=misc/ebuffer.cmo misc/log.cmo

htparse: lang.cmo $(HTMISC) $(JAPAN) $(HTML) html/htparse.cmo
	$(CAMLC) $(LINKFLAGS) -o $@ lang.cmo $(HTMISC) $(JAPAN) \
	  $(HTML) html/htparse.cmo

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


clean::
	rm -f version.cm* version.o
	rm -f lang.cm* lang.o
	rm -rf misc/*.cm* misc/*.o
	rm -rf www/*.cm* www/*.o
	rm -rf http/*.cm* http/*.o
	rm -rf html/*.cm* html/*.o
	rm -rf protos/*.cm* protos/*.o
	rm -rf retrieve/*.cm* retrieve/*.o
	rm -rf viewers/*.cm* viewers/*.o
	rm -rf htdisp/*.cm* htdisp/*.o
	rm -rf appsys/*.cm* appsys/*.o
	rm -rf safe/*.cm* safe/*.o
	rm -rf browser/*.cm* browser/*.o
	rm -rf japan/*.cm* japan/*.o
	rm -rf remote/*.cm* remote/*.o
	rm -f mmm.bin mmmx.bin mmm_remote htparse

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) $(DEBUG) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) $(DEBUG) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

depend: beforedepend
	(for d in misc; \
	do $(CAMLDEPPP) $(INCLUDES) $$d/*.mli $$d/*.ml; \
	done; \
        for d in www http html protos retrieve viewers htdisp appsys browser japan safe; \
	do $(CAMLDEP) $(INCLUDES) $$d/*.mli $$d/*.ml; \
	done; $(CAMLDEP) lang.ml lang.mli version.ml version.mli) > .depend
	cd sboard; $(MAKE) depend

include .depend

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


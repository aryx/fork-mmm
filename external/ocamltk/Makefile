include Makefile.config

SUBDIRS=compiler support lib frx jtk safe jpf tkanim tk80


all:	Makefile.config
	cd support; $(MAKE)
	cd compiler; $(MAKE)
	cd lib; $(MAKE) -f Makefile.gen; $(MAKE)
	cd frx; $(MAKE)
	cd jtk; $(MAKE)
#	cd threads; $(MAKE)
	cd jpf; $(MAKE)
	cd tkanim; $(MAKE)
	cd tk80; $(MAKE)
	cd safe; $(MAKE)

Makefile.config:
	@echo "You must configure first. Read INSTALL."
	exit 1

allopt: opt

opt:	Makefile.config
	cd support; $(MAKE) opt
	cd lib; $(MAKE) -f Makefile.gen; $(MAKE) opt
	cd frx; $(MAKE) opt
	cd jtk; $(MAKE) opt
	cd jpf; $(MAKE) opt
	cd tkanim; $(MAKE) opt
	cd tk80; $(MAKE) opt

lib: Widgets.src
	compiler/tkcompiler$(EXE) -outdir lib
	cd lib; $(MAKE)

top: lib/camltk.cma
	ocamlmktop -verbose -cclib -Lsupport -I lib -custom camltk.cma \
                -o camltktop
        # to launch: ./camltktop -I lib -I support

.PHONY: install

install: 
	test -d $(INSTALLDIR) || mkdir $(INSTALLDIR)
	/bin/rm -rf $(INSTALLDIR)/*
	cp Makefile.camltk $(INSTALLDIR)
	cd lib; $(MAKE) install
	cd support; $(MAKE) install
	cd compiler; $(MAKE) install
	cd frx; $(MAKE) install
	cd jpf; $(MAKE) install
	cd jtk; $(MAKE) install
	cd tkanim; $(MAKE) install
	cd tk80; $(MAKE) install
	cd safe; $(MAKE) install

uninstall:
	- rm -rf $(INSTALLDIR)

installopt: 
	test -d $(INSTALLDIR) || mkdir $(INSTALLDIR)
	cd lib; $(MAKE) installopt
	cd frx; $(MAKE) installopt
	cd jpf; $(MAKE) installopt
	cd jtk; $(MAKE) installopt
	cd tkanim; $(MAKE) installopt
	cd tk80; $(MAKE) installopt

clean : 
	-rm -f config.cache
	for d in $(SUBDIRS); do \
	cd $$d; $(MAKE) clean; cd ..; \
	done

distclean: clean
	cd tkanim; make distclean
	-rm -f config.log config.status config.cache
	-rm -f Makefile.config Makefile.camltk

# Just for camltk maintainers, distribution of the software
WEBSITEDIR=/net/pauillac/infosystems/www/camltk
FTPDIR=/net/pauillac/infosystems/ftp/cristal/caml-light/bazar-ocaml/ocamltk

# thread-tk is too old code: do not distribute

CAMLTKVERSION=418

distribute: tarfile website rpm
	$(MAKE) cleantar

cleantar:
	rm -rf release

website:
	rm -rf $(WEBSITEDIR)/*
	cd release; \
	cp -pr bazar-ocaml/camltk$(CAMLTKVERSION)/doc/* $(WEBSITEDIR); \
	ln -s $(WEBSITEDIR)/eng.htm $(WEBSITEDIR)/index.html
	- chgrp -R caml $(WEBSITEDIR)
	- chmod -R g+w $(WEBSITEDIR)

tarfile: cleantar
	mkdir release
	cd release; cvs co bazar-ocaml/camltk41; \
	rm -rf bazar-ocaml/camltk41/thread-tk; \
	find . -name '.cvsignore' -print | xargs rm; \
	find . -name 'CVS' -print | xargs rm -rf; \
	cp -p bazar-ocaml/camltk41/INSTALL bazar-ocaml/camltk41/doc/; \
	mv bazar-ocaml/camltk41 bazar-ocaml/camltk$(CAMLTKVERSION); \
	cd bazar-ocaml; \
	tar cvf ocamltk$(CAMLTKVERSION).tar camltk$(CAMLTKVERSION); \
	gzip ocamltk$(CAMLTKVERSION).tar; \
	mv -f ocamltk$(CAMLTKVERSION).tar.gz $(FTPDIR)
	chgrp caml $(FTPDIR)/ocamltk$(CAMLTKVERSION).tar.gz

rpm:
	if test -d /usr/src/redhat; then rpmdir=/usr/src/redhat; \
	else if test -d /usr/src/RPM; then rpmdir=/usr/src/RPM; fi; fi; \
	if test "X$$rpmdir" = "X"; then \
		echo "cannot create rpm"; exit 2; fi; \
	echo YOU NEED TO SU ROOT; \
	su root -c "cp $(FTPDIR)/ocamltk$(CAMLTKVERSION).tar.gz $$rpmdir/SOURCES/; rpm -ba --clean ./camltk.spec"; \
	cp $$rpmdir/SRPMS/camltk-0.$(CAMLTKVERSION)-1.src.rpm \
	   $$rpmdir/RPMS/*/camltk-0.$(CAMLTKVERSION)-1.*.rpm $(FTPDIR)

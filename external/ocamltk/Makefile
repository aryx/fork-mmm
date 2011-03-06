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

opt:	Makefile.config
	cd support; $(MAKE) opt
	cd lib; $(MAKE) -f Makefile.gen; $(MAKE) opt
	cd frx; $(MAKE) opt
	cd jtk; $(MAKE) opt
	cd jpf; $(MAKE) opt
	cd tkanim; $(MAKE) opt
	cd tk80; $(MAKE) opt

lib: Widgets.src
	compiler/tkcompiler
	cd lib; $(MAKE)

install: 
	test -d $(INSTALLDIR) || mkdir $(INSTALLDIR)
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

distclean:  clean
	-rm -f config.log config.status config.cache
	-rm -f Makefile.config Makefile.camltk

# Just for camltk maintainers, distribution of the software
WEBSITEDIR=/net/pauillac/infosystems/www/camltk
FTPDIR=/net/pauillac/infosystems/ftp/cristal/caml-light/bazar-ocaml/ocamltk

# thread-tk is too old code: do not distribute

distribute:
	rm -rf release
	rm -rf $(WEBSITEDIR)/*
	mkdir release
	cd release; cvs co bazar-ocaml/camltk41; \
	rm -rf bazar-ocaml/camltk41/thread-tk; \
	find . -name '.cvsignore' -print | xargs rm; \
	find . -name 'CVS' -print | xargs rm -rf; \
	cp -p bazar-ocaml/camltk41/INSTALL bazar-ocaml/camltk41/doc/; \
	cp -pr bazar-ocaml/camltk41/doc/* $(WEBSITEDIR); \
	ln -s $(WEBSITEDIR)/eng.htm $(WEBSITEDIR)/index.html
	- chgrp -R caml $(WEBSITEDIR)
	- chmod -R g+w $(WEBSITEDIR)
	cd release; mv bazar-ocaml/camltk41 camltk416; \
	tar cvf ocamltk416.tar camltk416; \
	gzip ocamltk416.tar; \
	mv -f ocamltk416.tar.gz $(FTPDIR)
	chgrp caml $(FTPDIR)/ocamltk416.tar.gz 	
	rm -rf release


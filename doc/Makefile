# for MMM homepage maintainers
# copy all the files to the MMM web site

MMMDIR=/net/pauillac/infosystems/www/mmm
VERSION=v418

install:
	rm -rf $(MMMDIR)/$(VERSION)
	mkdir $(MMMDIR)/$(VERSION)
	cp -r [a-z]* $(MMMDIR)/$(VERSION)/
	rm -f $(MMMDIR)/index.html $(MMMDIR)/index-fr.html $(MMMDIR)/jmmm/index.html
	cp index.html index-fr.html $(MMMDIR)
	cp index-ja.html $(MMMDIR)/jmmm/index.html

	- mkdir $(MMMDIR)/$(VERSION)/applets/
	cd ../applets; $(MAKE)
	cp ../applets/[a-z]* $(MMMDIR)/$(VERSION)/applets/

	- mkdir $(MMMDIR)/$(VERSION)/modules/
	cd ../modules; $(MAKE)
	cp ../modules/[a-z]* $(MMMDIR)/$(VERSION)/modules/

	chgrp -R caml $(MMMDIR)/index.html $(MMMDIR)/index-fr.html $(MMMDIR)/$(VERSION)
	chmod -R g+w $(MMMDIR)/index.html $(MMMDIR)/index-fr.html $(MMMDIR)/$(VERSION)

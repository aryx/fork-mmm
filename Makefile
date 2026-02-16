#############################################################################
# Dune
#############################################################################

.PHONY: all all.opt opt top clean distclean

all::
	dune build
clean::
	dune clean

mmm.opam:
	dune build $@
ocamltk.opam:
	dune build $@

#############################################################################
# Developer's targets
#############################################################################

visual:
	codemap -screen_size 3 -filter semgrep -efuns_client efuns_client -emacs_client /dev/null .

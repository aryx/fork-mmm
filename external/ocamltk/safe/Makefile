include ../Makefile.config

COMPFLAGS=-I ../support -I ../lib -I ../frx -I ../tkanim -I ../jtk

TARGETS=safetk.mli safetk.ml

all: $(TARGETS)

# support.ml / protocol.ml: defined by hand
# tk:
#   types extracted from tkgen.ml
#   functions from tkgen.mli (generated)
#   and from tkgen.ml (builtin)

include ../lib/modules


# Extract type definitions
# Add sharing
tktypes.ml: ../lib/tk.ml
	sed -n -e '/^(\* type \*)$$/,/^(\* \/type \*)$$/p' ../lib/tk.ml \
	| sed -e '/^(\* type \*)$$/d' -e '/^(\* \/type \*)$$/d' \
	> tktypes.ml

#	| sed -e 's/^type \([a-z][a-zA-Z_]*\) =$$/type \1 = Tk.\1 =/' \

tkfunctions.ml: ../lib/tk.ml
	(sed -n -e '/^FUNCTION$$/,/^\/FUNCTION$$/p' ../lib/tk.ml |\
         sed -e '/FUNCTION/d'; \
	cat ../lib/tkgen.mli) | \
	sed -e '/^(\* unsafe \*)$$/,/^(\* \/unsafe \*)$$/d' \
        > tkfunctions.ml

# Safetk is just a module of modules...
# Exported modules from ../support are: Widget, Protocol Textvariable Timer
# fileevent is not usable since it requires fds
# hashtblc and support are private
safetk.ml: tktypes.ml tkfunctions.ml
	(for i in Widget Protocol Textvariable Timer Tk;\
         do echo "module $$i = $$i"; done; \
	for i in $(WIDGETOBJS) ; do\
	 m=`basename $$i .cmo`;\
	 mm=`./capitalize $$m`;\
	 echo "module $$mm = $$mm";\
	done) > safetk.ml

# Modules from ../support have hand-made safe signatures
# Others are generated from annotations in Widgets.src
safetk.mli: widget.mli protocol.mli textvariable.mli timer.mli \
            tktypes.ml tkfunctions.ml 
	(cat widget.mli;\
	cat protocol.mli;\
	cat textvariable.mli;\
	cat timer.mli;\
	echo "module Tk : sig";\
          echo 'open Widget'; \
          echo 'open Textvariable'; \
	  cat tktypes.ml;\
	  cat tkfunctions.ml;\
	  echo "end";\
	for i in $(WIDGETOBJS) ; do\
	 m=`basename $$i .cmo`;\
	 mm=`./capitalize $$m`;\
	 echo "module $$mm : sig";\
	 sed -e '/^(\* unsafe \*)$$/,/^(\* \/unsafe \*)$$/d' ../lib/$$m.mli;\
	 echo "end";\
	 echo;\
	done) > safetk.mli


ITARGETS=safetk.mli safefrx.mli safejtk.mli safetkanim.mli \
	 safetk.ml safefrx.ml safejtk.ml safetkanim.ml

install: $(ITARGETS)
	cp $(ITARGETS) $(INSTALLDIR)

clean:
	rm -f *.cm? safetk.ml safetk.mli tktypes.ml tkfunctions.ml

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<


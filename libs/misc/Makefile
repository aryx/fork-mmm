TOP=../..

TARGET=lib

# Miscellaneous additional libraries
SRC= mstring.ml mlist.ml msys.ml \
     ebuffer.ml munix.ml \
     date.ml log.ml ibtree.ml lexpath.ml

INCLUDEDIRS=$(TOP)/libs/commons
SYSINCLUDES=

-include $(TOP)/Makefile.config
-include $(TOP)/Makefile.common

all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)
$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)


lexpath.ml : lexpath.mll
	$(CAMLLEX) lexpath.mll
clean::
	rm -f lexpath.ml
beforedepend:: lexpath.ml

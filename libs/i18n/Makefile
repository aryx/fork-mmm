TOP=../..

TARGET=lib

# Miscellaneous additional libraries
SRC= i18nprintf.ml lang.ml i18n.ml content_encoding.ml

SYSLIBS= str.cma unix.cma 
LIBS=
INCLUDEDIRS=
SYSINCLUDES=

-include $(TOP)/Makefile.config
-include $(TOP)/Makefile.common

all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa
$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)
$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

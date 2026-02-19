TOP=../..
TARGET=lib

SRC= fileevent_.ml timer_.ml low.ml condition.ml feed.ml error.ml hotlist.ml

SYSLIBS= str.cma unix.cma 
LIBS=
INCLUDEDIRS=$(TOP)/libs/commons $(TOP)/libs/i18n $(TOP)/libs/misc
SYSINCLUDES=

-include $(TOP)/Makefile.config
-include $(TOP)/Makefile.common

all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)
$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

TOP=../..

TARGET=lib

SRC= base64.ml lexdate.ml http_headers.ml \
     lexheaders.ml auth.ml \
     http.ml retype.ml \
     http_date.ml

SYSLIBS= str.cma unix.cma 
LIBS=

SSLDIR=$(shell ocamlfind query ssl)

INCLUDEDIRS=$(TOP)/libs/commons $(TOP)/libs/misc $(TOP)/libs/i18n \
   $(TOP)/src/core $(TOP)/src/url $(TOP)/src/www $(TOP)/src/globals \
   $(SSLDIR)

-include $(TOP)/Makefile.config
-include $(TOP)/Makefile.common

all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top


lexheaders.ml : lexheaders.mll
	$(CAMLLEX) lexheaders.mll
lexdate.ml : lexdate.mll
	$(CAMLLEX) lexdate.mll
clean::
	rm -f lexheaders.ml lexdate.ml
beforedepend:: lexheaders.ml lexdate.ml

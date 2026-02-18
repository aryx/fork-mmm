TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= Cap.ml CapStdlib.ml CapSys.ml CapUnix.ml \
 Eq.ml Ord.ml \
 Set_.ml Map_.ml \
 Common.ml \
 Dumper.ml OCaml.ml \
 Console.ml \
 Fpath.ml Fpath_.ml \
 Logs.ml Logs_.ml \
 Chan.ml FS.ml IO.ml Tmp.ml \
 Cmd.ml Proc.ml \
 Exception.ml Exit.ml \
 Arg_.ml \
 Endian.ml Arch.ml OS.ml \

#skipped Data already in libs/misc/
# Date.ml Bits.ml Rune.ml \

SYSLIBS= str.cma unix.cma 
LIBS=
INCLUDEDIRS=
SYSINCLUDES=

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
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


##############################################################################
# Literate Programming rules
##############################################################################

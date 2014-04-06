STCDIR ?= ../../..
DRIVER 		= all.tex
BUTFILES 	= importsall
include $(STCDIR)/lib/make/Makefile.vars
MODSLIBDIR = $(STCDIR)/lib
MATHFORMAT = pmml
all:	pdf
include $(STCDIR)/lib/make/Makefile.in
xhtml: $(TARGET.xhtml)
echo:	
	echo $(JOMDOCXSLPARAM)

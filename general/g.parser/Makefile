MODULE_TOPDIR = ../..

PGM = g.parser

LIBES = $(GISLIB)
DEPENDENCIES= $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd

#hardcoded here, this is an exeption due to the g.parser specialities:
htmlcmd1:
	-$(call htmldesc,$(ETC)/bin/cmd/$(PGM) test.sh)

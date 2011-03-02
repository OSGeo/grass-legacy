MODULE_TOPDIR = ../..

PGM = g.parser

LIBES = $(GISLIB)
DEPENDENCIES= $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd

#hardcoded here, this is an exeption due to the g.parser specialities:
ifneq ($(MSYSTEM),MINGW32)
htmlcmd:
	$(MAKE) $(HTMLDIR)/$(PGM).html HTMLSRC=
endif

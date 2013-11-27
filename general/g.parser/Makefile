MODULE_TOPDIR = ../..

PGM = g.parser

LIBES = $(GISLIB)
DEPENDENCIES= $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd
	$(INSTALL_DATA) g_parser_test.png $(HTMLDIR)/g_parser_test.png

#FIXME: please comment or file bug reports instead of just quietly
#  bypassing portability problems with a ifneq(). otherwise underlying
#  problems never get fixed. thanks
ifneq ($(MSYSTEM),MINGW32)

# hardcoded here, this is an exeption due to the g.parser specialities:
htmlcmd:
	$(MAKE) $(HTMLDIR)/$(PGM).html HTMLSRC=

endif

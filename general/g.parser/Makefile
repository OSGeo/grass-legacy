MODULE_TOPDIR = ../..

PGM = g.parser

LIBES = $(GISLIB)
DEPENDENCIES= $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd

#hardcoded here, this is an exeption due to the g.parser specialities:
htmlcmd:
	GRASS_FAKE_START=1 GISBASE=$(GISBASE) $(ETC)/bin/cmd/$(PGM) test.sh --html-description | grep -v '</body>' > $(PGM).html ; true
	@test ! -f description.html || ( cat description.html >> $(PGM).html )
	echo "<HR>" >> $(PGM).html
	echo "<P><a href=index.html>Help Index</a>" >> $(PGM).html
	echo "</body></html>" >> $(PGM).html
	mkdir -p $(GISBASE)/docs/html
	mv $(PGM).html $(GISBASE)/docs/html

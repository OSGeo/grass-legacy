MODULE_TOPDIR = ../..

PGM1=v.shape.register
PGM2=v.shape.unregister

PROGDIR =  $(GISBASE)/scripts/

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(PROGDIR)/$(PGM1) \
	$(PROGDIR)/$(PGM2) \
	htmlcmd

$(PROGDIR)/$(PGM1): $(PGM1)
	if [ ! -d $(PROGDIR) ]; then mkdir $(PROGDIR); fi
	cp $(PGM1) $(PROGDIR)
	chmod 0755 $(PROGDIR)/$(PGM1)

$(PROGDIR)/$(PGM2): $(PGM2)
	if [ ! -d $(PROGDIR) ]; then mkdir $(PROGDIR); fi
	cp $(PGM2) $(PROGDIR)
	chmod 0755 $(PROGDIR)/$(PGM2)

PROGRAMS = v.shape.register v.shape.unregister

htmlcmd:
	for prog in $(PROGRAMS) ; do \
		GRASS_FAKE_START=1 GISBASE=$(GISBASE) $(GISBASE)/scripts/$$prog --html-description | grep -v '</body>' > $$prog.tmp ; true ; \
		test ! -f $$prog.html || ( cat $$prog.html >> $$prog.tmp ) ; \
		echo "<HR>" >> $$prog.tmp ; \
		echo "<P><a href=index.html>Help Index</a>" >> $$prog.tmp ; \
		echo "</body></html>" >> $$prog.tmp ; \
		mkdir -p $(GISBASE)/docs/html ; \
		mv $$prog.tmp $(GISBASE)/docs/html/$$prog.html ; \
	done


MODULE_TOPDIR = ..

PGM1=v.shape.register
PGM2=v.shape.unregister
PGM3=postgisdb

PROGDIR =  $(GISBASE)/scripts/

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(PROGDIR)/$(PGM1) $(PROGDIR)/$(PGM2) $(PROGDIR)/$(PGM3)

$(PROGDIR)/$(PGM1): $(PGM1)
	if [ ! -d $(PROGDIR) ]; then mkdir $(PROGDIR); fi
	cp $(PGM1) $(PROGDIR)
	chmod 0755 $(PROGDIR)/$(PGM1)

$(PROGDIR)/$(PGM2): $(PGM2)
	if [ ! -d $(PROGDIR) ]; then mkdir $(PROGDIR); fi
	cp $(PGM2) $(PROGDIR)
	chmod 0755 $(PROGDIR)/$(PGM2)

$(PROGDIR)/$(PGM3): $(PGM3)
	if [ ! -d $(PROGDIR) ]; then mkdir $(PROGDIR); fi
	cp $(PGM3) $(PROGDIR)
	chmod 0755 $(PROGDIR)/$(PGM3)

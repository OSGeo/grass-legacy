
MODULE_TOPDIR = ..

PGM1=v.shape.register
PGM2=v.shape.unregister

PROGDIR =  $(GISBASE)/scripts/

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(PROGDIR)

$(PROGDIR): $(PGM1) $(PGM2)
	if [ ! -d $(PROGDIR) ]; then mkdir $(PROGDIR); fi
	cp $(PGM1) $(PGM2) $(PROGDIR)
	chmod 0755 $(PROGDIR)/$(PGM1)
	chmod 0755 $(PROGDIR)/$(PGM2)

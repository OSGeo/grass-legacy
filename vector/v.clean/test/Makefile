
MODULE_TOPDIR = ../../..

PGM = v.topo.check
RPOLY = $(GISBASE)/scripts/v.rand.poly
CTEST = $(GISBASE)/scripts/v.clean.test

LIBES = $(VECTLIB) $(DBMILIB) $(GISLIB) $(VASKLIB)
DEPENDENCIES = $(VECTDEP) $(DBMIDEP) $(GISDEP) $(VASKDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd $(RPOLY) $(CTEST)

$(RPOLY): v.rand.poly
	if [ ! -d $(GISBASE)/scripts ]; then mkdir $(GISBASE)/scripts; fi
	cp v.rand.poly $(RPOLY)
	chmod 0755 $(RPOLY)

$(CTEST): v.clean.test
	if [ ! -d $(GISBASE)/scripts ]; then mkdir $(GISBASE)/scripts; fi
	cp v.clean.test $(CTEST)
	chmod 0755 $(CTEST)


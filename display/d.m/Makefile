
MODULE_TOPDIR = ../..

PGM=d.m
PROG =  $(GISBASE)/scripts/$(PGM)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(PROG)

$(PROG): d.m
	if [ ! -d $(GISBASE)/scripts ]; then mkdir $(GISBASE)/scripts; fi
	cp d.m $(GISBASE)/scripts
	chmod 0755 $(PROG)

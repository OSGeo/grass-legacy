
MODULE_TOPDIR = ../..

PGM=v.format
PROG =  $(GISBASE)/scripts/$(PGM)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(PROG)


$(PROG): v.format.tcl 
	if [ ! -d $(GISBASE)/scripts ]; then mkdir $(GISBASE)/scripts; fi
	cp v.format.tcl $(PROG)
	chmod 0755 $(PROG)

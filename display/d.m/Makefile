
MODULE_TOPDIR = ../..

PGM=d.m
PROG =  $(GISBASE)/scripts/$(PGM)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(PROG)

TCL = cmd.tcl group.tcl print.tcl raster.tcl select.tcl  tool.tcl  tree.tcl  vector.tcl

$(PROG): *.gif *.tcl 
	if [ ! -d $(GISBASE)/etc/dm ]; then mkdir $(GISBASE)/etc/dm; fi
	cp *.gif $(GISBASE)/etc/dm/
	for tcl in $(TCL); do \
	    cp $$tcl $(GISBASE)/etc/dm/ ; \
	done
	if [ ! -d $(GISBASE)/scripts ]; then mkdir $(GISBASE)/scripts; fi
	cp d.m.tcl $(PROG)
	chmod 0755 $(PROG)


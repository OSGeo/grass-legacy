
MODULE_TOPDIR = ../..

PGM=d.m
PGM2=d.m.tcl
PROG1 =  $(GISBASE)/scripts/$(PGM)
PROG2 =  $(ETC)/dm/$(PGM2)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(PROG1) $(PROG2) htmlscript

TCL = cmd.tcl grassabout.tcl group.tcl print.tcl raster.tcl tksys.tcl tool.tcl tree.tcl vector.tcl labels.tcl

$(PROG1): $(PGM)
	if [ ! -d $(GISBASE)/scripts ]; then mkdir $(GISBASE)/scripts; fi
	cp $(PGM) $(PROG1)
	chmod 0755 $(PROG1)

$(PROG2): *.gif *.tcl 
	if [ ! -d $(ETC)/dm ]; then mkdir $(ETC)/dm; fi
	cp *.gif $(ETC)/dm/
	for tcl in $(TCL); do \
	    cp $$tcl $(ETC)/dm/ ; \
	done
	cp $(PGM2) $(PROG2)
	chmod 0755 $(PROG2)

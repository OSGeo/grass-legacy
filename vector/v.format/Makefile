
MODULE_TOPDIR = ../..

PGM=v.format
PGM2=v.format.tcl
PROG1 =  $(GISBASE)/scripts/$(PGM)
PROG2 =  $(ETC)/$(PGM2)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(PROG1) $(PROG2) htmlscript

$(PROG1): $(PGM)
	if [ ! -d $(GISBASE)/scripts ]; then mkdir $(GISBASE)/scripts; fi
	cp $(PGM) $(PROG1)
	chmod 0755 $(PROG1)

$(PROG2): $(PGM2)
	if [ ! -d $(ETC) ]; then mkdir $(ETC); fi
	cp $(PGM2) $(PROG2)
	chmod 0755 $(PROG2)

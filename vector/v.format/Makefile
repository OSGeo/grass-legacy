
MODULE_TOPDIR = ../..

PGM=v.format
PGM2=v.format.tcl
PROG1 =  $(GISBASE)/scripts/$(PGM)
PROG2 =  $(ETC)/$(PGM2)
EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(PROG1) $(PROG2) htmlscripts

$(PROG1): v.format 
	if [ ! -d $(GISBASE)/scripts ]; then mkdir $(GISBASE)/scripts; fi
	cp v.format $(PROG)
	chmod 0755 $(PROG)

$(PROG2): v.format.tcl
	if [ ! -d $(ETC) ]; then mkdir $(ETC); fi
	cp v.format.tcl $(PROG2)
	chmod 0755 $(PROG2)

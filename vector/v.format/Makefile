
MODULE_TOPDIR = ../..

PGM=v.format
PROG =  $(GISBASE)/scripts/$(PGM)
EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(PROG)


$(PROG): v.format.tcl 
	if [ ! -d $(GISBASE)/scripts ]; then mkdir $(GISBASE)/scripts; fi
	cp v.format.tcl $(PROG)
	chmod 0755 $(PROG)

MODULE_TOPDIR = ../..

PGM=i.oif

include $(MODULE_TOPDIR)/include/Make/Script.make

default: script
	$(MKDIR) $(GISBASE)/etc/i.oif/
	$(INSTALL) -m 755 i.oifcalc m.cutmatrix $(GISBASE)/etc/i.oif/

MODULE_TOPDIR = ../..

PGM = g3.createwind g3.list g3.remove g3.rename g3.setregion

include $(MODULE_TOPDIR)/include/Make/Script.make

default:
	for i in $(PGM) ; do $(INSTALL) -m 755 $$i $(GISBASE)/scripts/ ; done


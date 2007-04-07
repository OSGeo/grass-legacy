MODULE_TOPDIR = ../..

PGM=r.in.wms

include $(MODULE_TOPDIR)/include/Make/Script.make

default: script
	$(MKDIR) $(GISBASE)/etc/r.in.wms/
	$(INSTALL) -m 755 r.in.gdalwarp wms.request wms.download $(GISBASE)/etc/r.in.wms/

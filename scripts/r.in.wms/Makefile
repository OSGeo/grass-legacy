MODULE_TOPDIR = ../..

PGM=r.in.wms

include $(MODULE_TOPDIR)/include/Make/Script.make

default: script
	$(MKDIR) $(GISBASE)/etc/r.in.wms/
	cp r.in.gdalwarp wms.request wms.download $(GISBASE)/etc/r.in.wms/

MODULE_TOPDIR = ../..

SUBDIRS = \
	v.surf.bspline \
	v.outlier \
	v.lidar.correction \
	v.lidar.edgedetection \
	v.lidar.growing

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: parsubdirs

clean: cleansubdirs

$(SUBDIRS): lidarlib

lidarlib:
	$(MAKE) -C $@

.PHONY: lidarlib


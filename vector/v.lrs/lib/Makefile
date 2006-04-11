
MODULE_TOPDIR = ../../..

LIB_NAME = grass_lrs
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib $(GRASS_INCDIR)/lrs.h

$(GRASS_INCDIR)/lrs.h: lrs.h
	$(INSTALL) -m 644 lrs.h $(ARCH_INCDIR)/lrs.h



MODULE_TOPDIR = ../..

PGM=r3.mkdspf

LDIR = $(MODULE_TOPDIR)/lib/dspf
EXTRA_CFLAGS    =       -I$(LDIR) -I$(INCLUDE_DIR)

LIBES = $(DSPFLIB) $(G3DLIB) $(GISLIB) $(DATETIMELIB)
DEPENDENCIES = $(DSPFDEP) $(G3DDEP) $(GISDEP) $(DATETIMEDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd

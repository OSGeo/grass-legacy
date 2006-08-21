MODULE_TOPDIR = ../../..

PGM = v.lidar.edgedetection

LIBES = $(LIDARLIB) $(VECTLIB) $(GISLIB) $(MATHLIB) $(SEGMENTLIB) $(GMATHLIB)
DEPENDENCIES= $(LIDARDEP) $(VECTDEP) $(GISDEP) $(GMATHDEP) 
EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS) 

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd

 


MODULE_TOPDIR = ../../..

PGM = v.lrs.where

include $(MODULE_TOPDIR)/include/Make/Module.make

DEPENDENCIES = $(DISPLAYDEP) $(RASTERDEP) $(VECTDEP) $(DBMIDEP) $(GISDEP) \
               $(ARCH_LIBDIR)/$(LIB_PREFIX)grass_lrs.$(LIB_SUFFIX)

LIBES        = $(VECTLIB) \
               -lgrass_lrs \
               $(DBMILIB) $(GISLIB)

EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)

default: cmd


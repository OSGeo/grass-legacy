MODULE_TOPDIR = ../..

LIB_NAME = $(IMAGERY_LIBNAME)

ifneq ($(strip $(USE_CURSES)),)
  ADDVASKLIB = $(VASKLIB)
endif

EXTRA_LIBS = $(GMATHLIB) $(GISLIB) $(ADDVASKLIB)

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib


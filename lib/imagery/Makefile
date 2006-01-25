MODULE_TOPDIR = ../..

LIB_NAME = $(IMAGERY_LIBNAME)

include $(MODULE_TOPDIR)/include/Make/Lib.make

ifneq ($(strip $(USE_CURSES)),)
  ADDVASKLIB = $(VASKLIB)
endif

EXTRA_LIBS = $(GMATHLIB) $(GISLIB) $(ADDVASKLIB)

default: lib


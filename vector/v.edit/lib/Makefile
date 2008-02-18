MODULE_TOPDIR = ../../..

LIB_NAME = $(VEDIT_LIBNAME)
DEPENDENCIES = $(GISDEP)
EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)
EXTRA_LIBS = $(VECTLIB) $(DBMILIB) $(GISLIB)

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib $(ARCH_INCDIR)/vedit.h

$(ARCH_INCDIR)/vedit.h: vedit.h
	$(INSTALL_DATA) vedit.h $(ARCH_INCDIR)/vedit.h



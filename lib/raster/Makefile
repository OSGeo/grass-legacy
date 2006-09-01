MODULE_TOPDIR = ../..

PACKAGE ="libraster"
DEFS=-DPACKAGE=\"$(PACKAGE)\"
EXTRA_CFLAGS=$(GETHOSTNAME) $(FTINC) -I../pngdriver -I../driver $(PICFLAGS) $(DEFS)

EXTRA_LIBS=$(DRIVERLIB) $(GISLIB) $(FTLIB) $(ICONVLIB) $(PNGDRIVERLIB)
LIB_NAME = $(RASTER_LIBNAME)

include $(MODULE_TOPDIR)/include/Make/Lib.make
include $(MODULE_TOPDIR)/include/Make/Doxygen.make

default: lib ftcap

ftcap:
	@cp freetypecap $(ETC)/freetypecap

#doxygen:
DOXNAME=rastergraphics

MODULE_TOPDIR = ../..

EXTRA_LIBS=$(GISLIB) $(G3DLIB)
LIB_NAME = $(GPDE_LIBNAME)

include $(MODULE_TOPDIR)/include/Make/Lib.make
include $(MODULE_TOPDIR)/include/Make/Doxygen.make

default: headers lib

headers:
	for file in ./N_*.h ; do $(INSTALL_DATA) $$file $(GISBASE)/include/grass/ ; done

lib: headers

#doxygen:
DOXNAME=gpde

MODULE_TOPDIR = ../..

PGM=v.surf.rst

EXTRA_CLEAN_DIRS=doxygenhtml

LIBES     = $(INTERPDATALIB) $(QTREELIB) $(INTERPFLLIB) $(BITMAPLIB) $(LINKMLIB) \
            $(VECTLIB) $(VECTLIB_REAL) $(DBMILIB) $(GISLIB) $(DATETIMELIB)
 
include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd	

man:
	doxygen $(MODULE_TOPDIR)/Doxyfile

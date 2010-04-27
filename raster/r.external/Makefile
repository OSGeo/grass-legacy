MODULE_TOPDIR = ../..

PGM  = r.external

LIBES= $(GPROJLIB) $(GISLIB) $(GDALLIBS) $(GMATHLIB) $(IMAGERYLIB)
DEPENDENCIES = $(GPROJDEP) $(GISDEP) $(GMATHDEP) $(IMAGERYDEP)
EXTRA_INC = $(PROJINC) $(GDALCFLAGS)

include $(MODULE_TOPDIR)/include/Make/Module.make

ifneq ($(USE_GDAL),)
default: cmd
else
default:
endif

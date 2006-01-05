MODULE_TOPDIR = ../..

PGM = d.info

LIBES     = $(DISPLAYLIB) $(RASTERLIB) $(GISLIB)
DEPENDENCIES = $(DISPLAYDEP) $(RASTERDEP) $(GISDEP)

#screen scale support only if X11 Lib present:
ifneq ($(strip $(XLIB)),)
 LIBES += $(XLIB)
 EXTRA_CFLAGS = $(XLIBPATH) -DX11
endif

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd

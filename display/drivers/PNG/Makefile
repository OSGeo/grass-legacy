
MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make


EXTRA_CFLAGS = $(PNGINC) -I../lib

OBJS =  \
	Can_do.o\
	Clr_table.o\
	Color.o\
	Draw_line.o\
	Get_w_box.o\
	Get_w_line.o\
	Get_w_pnt.o\
	Graph_Clse.o\
	Graph_Set.o\
	Panel.o\
	Polygn_abs.o

ARCH_OBJS := $(foreach obj,$(OBJS),OBJ.$(ARCH)/$(obj))

LIBES = $(DRIVERLIB) $(DISPLAYLIB) $(RASTERLIB) $(GISLIB)
DEPENDENCIES = $(DRIVERLIBDEP) $(DRIVERDEP) $(RASTERDEP) $(GISLIBDEP)
HTMLPGM=pngdriver

default: $(DRIVERDIR)/PNG
	@if egrep '^PNG' $(ETC)/monitorcap >/dev/null 2>&1 ; then \
	echo PNG already defined in $(ETC)/monitorcap ; \
	else \
	echo adding PNG driver to $(ETC)/monitorcap ; \
	cat moncap.entry >> $(ETC)/monitorcap ; \
	fi
	cat description.html > $(HTMLPGM).html
	echo "<HR>" >> $(HTMLPGM).html
	echo "<P><a href=index.html>Help Index</a>" >> $(HTMLPGM).html
	echo "</body></html>" >> $(HTMLPGM).html
	$(MKDIR) $(GISBASE)/docs/html
	$(INSTALL) -m 644 $(HTMLPGM).html $(GISBASE)/docs/html


$(DRIVERDIR)/PNG: $(ARCH_OBJS) $(DEPENDENCIES)
	$(CC) $(LDFLAGS) -o $@ $(XLDFLAGS) $(LIBPATH) $(ARCH_OBJS) $(LIBES) $(PNGLIB) $(XLIBPATH) $(XLIB) $(XEXTRALIBS) $(MATHLIB) $(XDRLIB)

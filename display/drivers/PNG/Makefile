MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

EXTRA_CFLAGS = $(PNGINC) -I../lib

OBJS =  \
	Box.o\
	Can_do.o\
	Client.o\
	Color.o\
	Color_table.o\
	Draw_line.o\
	Draw_point.o\
	Erase.o\
	Graph_close.o\
	Graph_set.o\
	Line_width.o\
	Raster_int.o\
	Respond.o\
	main.o\
	write.o

ARCH_OBJS := $(foreach obj,$(OBJS),OBJ.$(ARCH)/$(obj))

LIBES = $(DRIVERLIB) $(GISLIB) $(FTLIB) $(ICONVLIB)
DEPENDENCIES = $(DRIVERDEP) $(GISLIBDEP)
PGM=pngdriver

default: $(DRIVERDIR)/PNG htmlgen moncap

moncap:
	@if [ -f $(ETC)/monitorcap ] && egrep '^PNG' $(ETC)/monitorcap >/dev/null 2>&1 ; then \
	echo PNG already defined in $(ETC)/monitorcap ; \
	else \
	echo adding PNG driver to $(ETC)/monitorcap ; \
	cat moncap.entry >> $(ETC)/monitorcap ; \
	fi

$(DRIVERDIR)/PNG: $(ARCH_OBJS) $(DEPENDENCIES)
	$(CC) $(LDFLAGS) -o $@ $(XLDFLAGS) $(LIBPATH) $(ARCH_OBJS) $(LIBES) $(PNGLIB) $(XLIBPATH) $(XLIB) $(XEXTRALIBS) $(MATHLIB) $(XDRLIB)


MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

EXTRA_CFLAGS = -I../lib

OBJS =  \
	Can_do.o\
	Color.o\
	Draw_line.o\
	Get_w_box.o\
	Get_w_line.o\
	Get_w_pnt.o\
	Graph_Clse.o\
	Graph_Set.o\
	Panel.o\
	Polygn_abs.o\
	Reset_clr.o \
	Text.o

ARCH_OBJS := $(foreach obj,$(OBJS),OBJ.$(ARCH)/$(obj))

LIBES = $(DRIVERLIB) $(DISPLAYLIB) $(RASTERLIB) $(GISLIB)
DEPENDENCIES = $(DRIVERLIBDEP) $(DRIVERDEP) $(DISPLAYDEP) $(RASTERDEP) $(GISLIBDEP)
PGM=htmlmapdriver

default: $(DRIVERDIR)/HTMLMAP htmlgen
	@if egrep '^HTMLMAP' $(ETC)/monitorcap >/dev/null 2>&1 ; then \
	echo HTMLMAP already defined in $(ETC)/monitorcap ; \
	else \
	echo adding HTMLMAP driver to $(ETC)/monitorcap ; \
	cat moncap.entry >> $(ETC)/monitorcap ; \
	fi


$(DRIVERDIR)/HTMLMAP: $(ARCH_OBJS) $(DEPENDENCIES)
	$(CC) $(LDFLAGS) -o $@ $(XLDFLAGS) $(LIBPATH) $(ARCH_OBJS) $(LIBES) $(XLIBPATH) $(XLIB) $(XEXTRALIBS) $(MATHLIB) $(XDRLIB)


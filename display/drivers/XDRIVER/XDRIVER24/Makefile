
MODULE_TOPDIR = ../../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

LIBES = $(DRIVERLIB) $(GISLIB) $(FTLIB) $(ICONVLIB)
DEPENDENCIES = $(DRIVERDEP) $(GISDEP)

# if you are compiling under X11R3 add this define
#R3DEFINE=-DX11R3
# and comment out this line
R3DEFINE=

# For those compiling on Intergraph machines, uncomment the following 2 lines:
#XCFLAGS=-DINTERGRAPH -DX11R3

# these are for the CDC mips machine
#XCFLAGS=-systype bsd43
#XLDFLAGS=-systype bsd43

# One user reported that he had to load -lsockstub for SCO-UNIX under X11R3
# we record his comment here for anyone that may have problems under SCO-UNIX
# This problem was NOT reported when CWU did the port to SCO-UNIX
#XEXTRALIBS=-lsockstub

# On the Silcon Graphics Boxes, the standard XOpenDisplay() fails when
# trying to display to another machine if the machine is named on
# the DISPLAY variable instead of it IP address. Loading the -lsun
# library resolves this problem
#XEXTRALIBS=-lsun

EXTRA_CFLAGS=$(XCFLAGS) $(R3DEFINE) -I../../lib

OBJS =  \
	Box_abs.o\
	Can_do.o\
	Client.o\
	Clr_table.o\
	Color.o\
	Draw_line.o\
	Get_w_box.o\
	Get_w_line.o\
	Get_w_pnt.o\
	Graph_Clse.o\
	Graph_Set.o\
	Panel.o\
	Plylne_abs.o\
	Polygn_abs.o\
	Raster.o\
	Raster_RGB.o\
	Respond.o\
	Serve_Xevent.o\
	Work.o\
	alloc.o\
	Draw_point.o

ARCH_OBJS := $(foreach obj,$(OBJS),OBJ.$(ARCH)/$(obj))

PGM=xdriver

#FREETYPEBASED = Draw_point.o
#compile if FreeType Lib present:
#ifneq ($(strip $(FTLIB)),)
#    OBJS += $(FREETYPEBASED)
#endif

default: $(DRIVERDIR)/XDRIVER htmlgen

$(DRIVERDIR)/XDRIVER: $(ARCH_OBJS) $(DEPENDENCIES) $(ARCH_INCDIR)/winname.h
	$(CC) $(LDFLAGS) -o $@ $(XLDFLAGS) $(LIBPATH) $(ARCH_OBJS) $(LIBES) $(XLIBPATH) $(XLIB) $(XEXTRALIBS) $(MATHLIB) $(XDRLIB)

$(ARCH_INCDIR)/winname.h:
	echo '#define WIN_NAME "GRASS $(VERSION_NUMBER)"' > $@


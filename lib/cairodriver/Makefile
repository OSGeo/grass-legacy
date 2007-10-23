MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Platform.make

PACKAGE ="libcairodriver"
DEFS=-DPACKAGE=\"$(PACKAGE)\"

CAIRO_CFLAGS:=$(shell pkg-config --cflags cairo)
EXTRA_CFLAGS=-I../driver $(CAIRO_CFLAGS) $(PICFLAGS) $(DEFS)

CAIRO_LIBS:=$(shell pkg-config --libs cairo)
EXTRA_LIBS=$(DRIVERLIB) $(GISLIB) $(CAIRO_LIBS)

# added constant to /include/Make/Grass.make.in
LIB_NAME = $(CAIRODRIVER_LIBNAME)

LIB_OBJS = \
	Driver.o	\
	Client.o	\
	Box.o		\
	Graph.o		\
	Erase.o		\
	Set_window.o	\
	Line_width.o	\
	Draw_line.o	\
	Draw_bitmap.o	\
	Draw_point.o	\
	Poly.o		\
	Raster.o	\
	Respond.o	\
	Color.o		\
	write.o

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib

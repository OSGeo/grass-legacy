MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Platform.make

EXTRA_CFLAGS=$(FTINC)
EXTRA_LIBS=$(GISLIB) $(FTLIB) $(ICONVLIB)
LIB_NAME = $(DRIVER_LIBNAME)

LIB_OBJS =\
	Box.o\
	Can_do.o\
	Client.o\
	Color_table.o\
	Color.o\
	Cont.o\
	Draw.o\
	Erase.o\
	Font.o\
	Get_location.o\
	Get_t_box.o\
	Graph.o\
	Line_width.o\
	Move.o\
	Num_colors.o \
	Panel.o\
	Polydots.o\
	Polyline.o\
	Polygon.o\
	Raster_RGB.o\
	Raster_int.o\
	Raster_char.o\
	Reset_colors.o\
	Respond.o\
	Returns.o\
	Set_window.o\
	Text.o\
	Text_size.o\
	Work.o\
	clip.o\
	color_support.o\
	font2.o\
	font_freetype.o\
	init.o\
	pad.o\
	text2.o\
	text3.o

ifneq ($(HAVE_SOCKET),)
  LIB_OBJS += \
	main.o\
	command.o\
	connect_fifo.o\
	connect_sock.o
endif

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib


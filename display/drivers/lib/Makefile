MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Platform.make

EXTRA_CFLAGS=$(FTINC)
EXTRA_LIBS=$(GISLIB)
LIB_NAME = $(DRIVER_LIBNAME)

LIB_OBJS =\
	Box_abs.o\
	Box_rel.o\
	Client.o\
	Clr_table.o\
	Color.o\
	Cont_abs.o\
	Cont_rel.o\
	Erase.o\
	Font_get.o\
	Get_t_box.o\
	Linemod.o\
	Move_abs.o\
	Move_rel.o\
	Num_colors.o \
	Plydts_abs.o\
	Plydts_rel.o\
	Plylne_abs.o\
	Plylne_rel.o\
	Polygn_rel.o\
	Raster_int.o\
	Raster_chr.o\
	Raster_def.o\
	Reset_clrs.o\
	Respond.o\
	Returns.o\
	Set_window.o\
	Text.o\
	Text_size.o\
	Work.o\
	clip.o\
	color_supp.o\
	command.o\
	connect_fifo.o\
	connect_sock.o\
	font.o\
	main.o\
	pad.o\
	utils.o\
	RGB.o\
	Text2.o\
	Text3.o\
	font_freetype.o

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: stlib



MODULE_TOPDIR = ../../..

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
	Text2.o

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib

$(OBJDIR)/Cont_abs.o: driver.h
$(OBJDIR)/Cont_rel.o: driver.h
$(OBJDIR)/Get_t_box.o: driver.h
$(OBJDIR)/Move_abs.o: driver.h
$(OBJDIR)/Move_rel.o: driver.h
$(OBJDIR)/Plylne_rel.o: driver.h
$(OBJDIR)/Polygn_rel.o: driver.h
$(OBJDIR)/Raster.o: driver.h
$(OBJDIR)/Text.o: driver.h
$(OBJDIR)/Text_size.o: driver.h

MODULE_TOPDIR = ../../../..

OBJS = \
	do_dots.o\
	do_line.o\
	find_area.o\
	ply_t_bmif.o\
	save_area.o\
	wrte_recrd.o\
	yadjust.o

LIBES = $(GISLIB)
DEPENDENCIES= $(DEPGISLIB)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(BIN)/bmif_to_cell $(BIN)/poly_to_bmif

$(BIN)/bmif_to_cell: bmf_to_cll.o $(DEPENDENCIES)
	$(CC) $(LDFLAGS) -o $@ bmf_to_cll.o $(LIBES) $(MATHLIB) $(XDRLIB)

$(BIN)/poly_to_bmif: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS)

OBJS: ply_to_cll.h

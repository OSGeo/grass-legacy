MODULE_TOPDIR = ../../../..

PGM=poly_to_bmif

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

default: cmd

htmlcmd:
	@echo "nothing to do"


MODULE_TOPDIR = ../..

PGM = g.mapsets
GUI = $(GISBASE)/etc/g.mapsets.tcl

LIBES     = $(GISLIB) $(VASKLIB)
DEPENDENCIES= $(GISDEP) $(VASKDEP)

INTER_OBJS = \
	main_inter.o

#	dsply_path.o\
#	dsply_maps.o\
#	get_maps.o\
#	get_path.o\
#	set_path.o\
#	scan_int.o

CMD_OBJS = \
	main_cmd.o \
	dsply_path.o \
	dsply_maps.o \
	get_maps.o

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd inter $(GUI)

$(GUI): g.mapsets.tcl
	if [ ! -d $(GISBASE)/etc ]; then mkdir $(GISBASE)/etc; fi
	cp g.mapsets.tcl $(GUI)
	chmod 0755 $(GUI)

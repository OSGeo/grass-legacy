MODULE_TOPDIR = ../..

PGM = r.colors

LIST=main.o\
	eq.o\
	log.o\
	rules.o

COLORFILES = aspect byg byr grey gyr rainbow ramp ryg terrain wave

LIBES = $(GISLIB)
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd $(ETC)/colors

$(ETC)/colors: $(COLORFILES)
	-mkdir $@
	cp -f $^ $@

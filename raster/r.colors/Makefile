MODULE_TOPDIR = ../..

PGM = r.colors

LIST=main.o\
	eq.o\
	log.o\
	rules.o

COLORFILES = aspect byg byr grey gyr rainbow ramp ryg wave

LIBES = $(GISLIB)
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(ETC)/colors cmd

$(ETC)/colors: $(COLORFILES)
	-mkdir $@
	cp -f $^ $@

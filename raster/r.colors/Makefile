MODULE_TOPDIR = ../..

PGM = r.colors

LIST=main.o\
	eq.o\
	log.o\
	rules.o

COLRFILES = aspect bcyr byg byr elevation grey gyr rainbow ramp ryg slope terrain wave

LIBES = $(GISLIB)
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(ETC)/colors cmd

$(ETC)/colors: $(COLRFILES)
	$(MKDIR) $@
	$(INSTALL) -m 644 $^ $@

MODULE_TOPDIR = ../..

PGM = r.colors

LIST=main.o\
	eq.o\
	log.o\
	rules.o

COLRFILES = aspect bcyr byg byr elevation evi etopo2 grey gyr ndvi rainbow ramp ryg slope srtm terrain wave

LIBES = $(GISLIB)
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(ETC)/colors cmd

$(ETC)/colors: $(COLRFILES)
	$(MKDIR) $@
	$(INSTALL) -m 644 $^ $@

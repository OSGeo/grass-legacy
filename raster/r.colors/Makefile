MODULE_TOPDIR = ../..

PGM = r.colors

LIST=main.o\
	eq.o\
	log.o\
	rules.o

COLRFILES = aspect aspectcolr bcyr byg byr curvature differences elevation etopo2 evi grey gyr ndvi population rainbow ramp ryb ryg slope srtm terrain wave

LIBES = $(GISLIB)
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(ETC)/colors cmd

$(ETC)/colors: $(COLRFILES)
	$(MKDIR) $@
	$(INSTALL) -m 644 $^ $@

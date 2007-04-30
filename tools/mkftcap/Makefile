MODULE_TOPDIR = ../..

PGM = mkftcap

CAPFILE = $(ETC)/freetypecap

include $(MODULE_TOPDIR)/include/Make/Script.make

default: script $(CAPFILE)

$(CAPFILE): $(PROGDIR)/$(PGM)
	$<

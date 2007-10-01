
MODULE_TOPDIR = ../..

SUBDIRS = \
	r.sim.water \
	r.sim.sediment

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: parsubdirs

clean: cleansubdirs

$(SUBDIRS): simlib

simlib:
	$(MAKE) -C $@

.PHONY: simlib


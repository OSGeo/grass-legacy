MODULE_TOPDIR = ..

PGM1=timer

PROGDIR =  $(GISBASE)/scripts/

include $(MODULE_TOPDIR)/include/Make/Module.make

PGM1:
	$(MAKE) -C timer

default: PGM1

clean:
	-rm -rf $(OBJDIR)
	$(MAKE) -C timer clean

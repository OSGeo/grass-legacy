MODULE_TOPDIR = ..

PGM1=timer
PGM2=getModTime

PROGDIR =  $(GISBASE)/scripts/

include $(MODULE_TOPDIR)/include/Make/Module.make

PGM1:
	$(MAKE) -C timer

PGM2:
	$(MAKE) -C getModTime

default: PGM1 PGM2

clean:
	-rm -rf $(OBJDIR)
	$(MAKE) -C timer clean
	$(MAKE) -C getModTime clean
